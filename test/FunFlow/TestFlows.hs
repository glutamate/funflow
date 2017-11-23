{-# LANGUAGE Arrows      #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE QuasiQuotes #-}

module FunFlow.TestFlows where

import           Control.Arrow
import           Control.Exception
import           Control.FunFlow.Base
import qualified Control.FunFlow.ContentStore                as CS
import           Control.FunFlow.Steps
import           Control.Monad                               (when)
import           Path
import           Path.IO
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.FunFlow.Exec.Redis
import           Control.FunFlow.Exec.Simple
import           Control.FunFlow.External.Coordinator.Memory

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

flowAssertions :: [FlowAssertion]
flowAssertions =
  [ FlowAssertion "death" "foo" melancholicLazarus Nothing setup
  , FlowAssertion "resurrection" "bar" (retry 1 0 melancholicLazarus) (Just "bar") setup
  , FlowAssertion "bernoulli_once" 0.2 (retry 20 0 $ worstBernoulli mkError >>^ (<2.0)) (Just True) (return ())
  , FlowAssertion "bernoulli_twice" () (flow2caught >>^ snd >>^ (<2.0)) (Just True) (return ())
  , FlowAssertion "failStep" () failStep Nothing (return ())
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
      res <- runSimpleFlow MemoryCoordinator hook store flw x
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
