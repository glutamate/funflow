{-# LANGUAGE Arrows            #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Funflow.TestFlows where

import           Control.Arrow
import           Control.Arrow.Free
import           Control.Concurrent.Async                    (withAsync)
import           Control.Exception.Safe                      hiding (catch)
import           Control.Funflow
import           Control.Funflow.ContentStore                (Content ((:</>)))
import qualified Control.Funflow.ContentStore                as CS
import           Control.Funflow.External.Coordinator.Memory
import           Control.Funflow.External.Executor           (executeLoop)
import           Control.Monad                               (when)
import           Data.Default                                (def)
import           Data.List                                   (sort)
import           Path
import           Path.IO
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit

data FlowAssertion where
  FlowAssertion :: (Eq b, Show b)
                => String -- test name
                -> a  -- input
                -> SimpleFlow a b -- the flow to test
                -> Maybe b --expected output - Nothing for expected failure
                -> IO () -- test setup action
                -> FlowAssertion

mkError :: String -> SomeException
mkError = toException . userError

flow2 :: SimpleFlow () (Double,Double)
flow2 = proc () -> do
  r1 <- worstBernoulli mkError -< 0.2
  r2 <- worstBernoulli mkError -< 0.3
  returnA -< (r1,r2)

flow2caught :: SimpleFlow () (Double,Double)
flow2caught = retry 100 0 flow2

aliasFlow :: SimpleFlow () (Maybe String, Maybe String)
aliasFlow = proc () -> do
  let alias = CS.Alias "alias"
  mb1 <- lookupAliasInStore -< alias
  r1 <- case mb1 of
    Nothing -> do
      item :</> _path <- writeString_ -< "test"
      assignAliasInStore -< (alias, item)
      returnA -< Nothing
    Just item ->
      arr Just <<< readString_ -< item
  mb2 <- lookupAliasInStore -< alias
  r2 <- case mb2 of
    Nothing ->
      returnA -< Nothing
    Just item ->
      arr Just <<< readString_ -< item
  returnA -< (r1, r2)

flowCached :: SimpleFlow () Bool
flowCached = let
    randomStep = stepIO' (def { cache = $(defaultCacherLoc (0 :: Int))}) $ const (randomIO :: IO Int)
  in proc () -> do
    t1 <- randomStep -< ()
    t2 <- randomStep -< ()
    returnA -< (t1 == t2)

-- | Test that we can merge directories within the content store.
flowMerge :: SimpleFlow () Bool
flowMerge = proc () -> do
  f1 <- writeString -< ("Hello World",[relfile|a|] )
  f2 <- writeString -< ("Goodbye World", [relfile|b|])
  comb <- mergeFiles -< [f1, f2]
  files <- arr (fmap CS.contentFilename) <<< arr snd <<< listDirContents -< comb
  returnA -< (sort files == sort [[relfile|a|], [relfile|b|]])

-- | Test that a missing executable in an external causes a catchable error.
flowMissingExecutable :: SimpleFlow () (Either () ())
flowMissingExecutable = proc () -> do
  r <- (arr Right <<< external (\() -> ExternalTask
    { _etCommand = "non-existent-executable-39fd1e85a0a05113938e0"
    , _etParams = []
    , _etWriteToStdOut = StdOutCapture
    , _etEnv = EnvExplicit []
    }))
    `catch` arr (Left @SomeException . snd)
    -< ()
  returnA -< case r of
    Left _ -> Left ()
    Right _ -> Right ()

-- | Test that we can provide an environment variable to an external step.
externalEnvVar :: SimpleFlow () (Either String ())
externalEnvVar = proc () -> do
  r <- readString_ <<< external (\() -> ExternalTask
    { _etCommand = "bash"
    , _etParams = [textParam "-c", textParam "echo -n $FOO"]
    , _etWriteToStdOut = StdOutCapture
    , _etEnv = EnvExplicit [("FOO", textParam "testing")]
    }) -< ()
  returnA -< case r of
    "testing" -> Right ()
    x -> Left x

flowAssertions :: [FlowAssertion]
flowAssertions =
  [ FlowAssertion "death" "foo" melancholicLazarus Nothing setup
  , FlowAssertion "resurrection" "bar" (retry 1 0 melancholicLazarus) (Just "bar") setup
  , FlowAssertion "bernoulli_once" 0.2 (retry 20 0 $ worstBernoulli mkError >>^ (<2.0)) (Just True) (return ())
  , FlowAssertion "bernoulli_twice" () (flow2caught >>^ snd >>^ (<2.0)) (Just True) (return ())
  , FlowAssertion "failStep" () failStep Nothing (return ())
  , FlowAssertion "aliasFlow" () aliasFlow (Just (Nothing, Just "test")) (return ())
  , FlowAssertion "cachingFlow" () flowCached (Just True) (return ())
  , FlowAssertion "mergingStoreItems" () flowMerge (Just True) (return ())
  , FlowAssertion "missingExecutable" () flowMissingExecutable (Just (Left ())) (return ())
  , FlowAssertion "externalEnvVar" () externalEnvVar (Just (Right ())) (return ())
  ]

setup :: IO ()
setup = do ex <- doesFileExist [absfile|/tmp/lazarus_note|]
           when ex $ removeFile [absfile|/tmp/lazarus_note|]

testFlowAssertion :: FlowAssertion -> TestTree
testFlowAssertion (FlowAssertion nm x flw expect before) =
  testCase nm $
    withSystemTempDir "test_output_" $ \storeDir ->
    CS.withStore storeDir $ \store -> do
      hook <- createMemoryCoordinator
      before
      res <- withAsync (executeLoop MemoryCoordinator hook store) $ \_ ->
        runSimpleFlow MemoryCoordinator hook store flw x
      assertFlowResult expect res

assertFlowResult :: (Eq a, Show ex, Show a) => Maybe a -> Either ex a -> Assertion
assertFlowResult expect res =
    case (expect, res) of
      (Nothing, Left _) -> return ()
      (Just xr, Right y) -> assertEqual "flow results" xr y
      (Nothing, Right y) -> assertFailure $ "expected flow failure, got success" ++ show y
      (Just xr, Left err) -> assertFailure $ "expected success "++ show xr++", got error" ++ show err

tests :: TestTree
tests = testGroup "Flow Assertions" $ map testFlowAssertion flowAssertions
