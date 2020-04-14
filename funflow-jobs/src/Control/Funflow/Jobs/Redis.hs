{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Distributable Funflow jobs. This allows full jobs (e.g. workflows) to be
--   distributed using Redis. This is at the cost of requiring the same code to
--   be running everywhere, and having a map from strings to codepoints.
module Control.Funflow.Jobs.Redis
  ( Job(..)
  , JobId
  , JobStatus(..)
  , RFlowM
  , FlowST
  , resumeJob
  , sparkJob
  , queueLoop
  , getJobById
  , getJobsByStatus
  , runRFlow
  )  where

import           Control.Concurrent
import           Control.Funflow
import           Control.Lens                hiding (argument)
import           Control.Monad               (void)
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS8
import           Data.Either                 (rights)
import           Data.Maybe                  (catMaybes)
import           Data.Monoid                 ((<>))
import           Data.Store
import qualified Data.Text                   as T
import qualified Database.Redis              as R
import           GHC.Generics
import           UnliftIO                    (MonadUnliftIO, withRunInIO, wrappedWithRunInIO)

type NameSpace = ByteString

type RFlowM a = ExceptT String (StateT FlowST R.Redis) a

instance MonadBase IO R.Redis where
  liftBase = liftIO

instance MonadUnliftIO R.Redis where
  withRunInIO = wrappedWithRunInIO R.reRedis R.unRedis

instance MonadThrow R.Redis where
  throwM = liftIO . throwM

instance MonadCatch R.Redis where
  catch x y = R.reRedis
            $ Control.Monad.Catch.catch (R.unRedis x) (R.unRedis <$> y)

type FlowST = (NameSpace, R.Connection)

-- | Run the RFlowM monad
runRFlow :: R.Connection -> RFlowM a -> IO (Either String a)
runRFlow conn mx = R.runRedis conn
  $ evalStateT (runExceptT mx) ("", conn)

-- | Use redis commands inside RFlowM
redis :: R.Redis (Either R.Reply a) -> RFlowM a
redis r = do
  ex <- lift $ lift r
  case ex of
    Left rply -> throwError $ "redis: " ++ show rply
    Right x   -> return x

type JobId = Integer

data JobStatus
  = JobDone
  | JobError
  | JobRunning
  | JobQueue
  deriving (Generic, Show)

data Job a b = Job
  { jobId     :: JobId
  , taskName  :: T.Text
  , jobStatus :: JobStatus
  , jobError  :: Maybe String
  , argument  :: a
  , result    :: Maybe b
  } deriving (Generic)

instance Store JobStatus

instance (Store a, Store b) => Store (Job a b)

-- | Create a job in the waiting queue without actually running it
sparkJob :: forall a. Store a => T.Text -> a -> RFlowM JobId
sparkJob nm x = do
  jid :: JobId <- redis $ R.incr "jobfresh"
  let job  = Job jid nm JobQueue Nothing x Nothing :: Job a ()
  _ <- redis $ R.set (BS8.pack $ "job_" ++ show jid) (encode job)
  _ <- redis $ R.rpush "jobs_queue" [encode jid]
  return jid

-- | Get all the jobs by status
getJobsByStatus :: (Store a,Store b) => JobStatus -> RFlowM [Job a b]
getJobsByStatus js = do
  let queueNm =
        case js of
          JobRunning -> "jobs_running"
          JobQueue   -> "jobs_queue"
          JobDone    -> "jobs_done"
          JobError   -> "jobs_error"
  jids <- map decode <$> redis (R.lrange queueNm 0 (-1))
  fmap catMaybes $ mapM getJobById $ rights jids

-- | Get a job by job ID
getJobById :: (Store a,Store b) => JobId -> RFlowM (Maybe (Job a b))
getJobById jid = do
  let jobIdNm = BS8.pack $ "job_" ++ show jid
  mjob <- redis $ R.get jobIdNm
  case mdecode mjob of
    Left err -> do liftIO $ putStrLn $ "job error: "++err
                   return Nothing
    Right job -> return $ Just job

-- | Loop forever, looking for new jobs that have been put on the waiting queue, and run them.
queueLoop ::
     forall a b eff ex. (Store a, Store b, Exception ex)
  => [(T.Text, Flow eff ex a b)]
  -> (Flow eff ex a b -> a -> IO (Either String b) )
  -> RFlowM ()
queueLoop allJobs runner = forever go
  where
    go = do
      mkj <- redis $ R.brpoplpush "jobs_queue" "job_running" 1
      whenRight (mdecode mkj) $ \jid -> do
        liftIO $ threadDelay 100000
        mjob <- getJobById jid
        --liftIO $ putStrLn $ "queueLoop got job id "++ show (jid,fmap jobId mjob)
        whenJust mjob $ void . resumeJob allJobs runner

-- | Resume a job
resumeJob ::
     forall a b eff ex. (Store a, Store b, Exception ex)
  => [(T.Text, Flow eff ex a b)]
  -> (Flow eff ex a b -> a -> IO (Either String b) )
  -> Job a b
  -> RFlowM (Either String b)
resumeJob allJobs runner job = case lookup (taskName job) allJobs of
  Just flow -> do
    --liftIO $ putStrLn $ "resumeJob got job id "++ show (jobId job)
    let jobIdNm = BS8.pack $ "job_" ++ show (jobId job)
    _1 .= jobIdNm
    eres <- liftIO $ runner flow (argument job)
    finishJob job eres
    return eres
  Nothing -> return $ Left $ "Failed to lookup job with name "
                          <> T.unpack (taskName job)

-- | When a job has finished, mark it as done and put it on the done or error queues
finishJob :: (Store a, Store b) => Job a b -> Either String b -> RFlowM ()
finishJob job y = do
  let jid = jobId job
  --liftIO $ putStrLn $ "finish got job id "++ show jid
  let jobIdNm = BS8.pack $ "job_" ++ show jid
  let newJob =
        case y of
          Right res -> job {jobStatus = JobDone, result = Just res}
          Left err  -> job {jobStatus = JobError, jobError = Just err}
  _ <- redis $ R.set jobIdNm (encode newJob)
  _ <- redis $ R.lrem "jobs_running" 1 (encode jid)
  _ <- case y of
    Right _ -> redis $ R.rpush "jobs_done" [encode jid]
    Left _  -> redis $ R.rpush "jobs_error" [encode jid]
  return ()

mdecode :: Store a => Maybe ByteString -> Either String a
mdecode (Nothing) = Left "no value"
mdecode (Just bs) = over _Left show $ decode bs

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

whenRight :: Monad m => Either b a -> (a -> m ()) -> m ()
whenRight (Left _) _ = return ()
whenRight (Right x) f = f x
