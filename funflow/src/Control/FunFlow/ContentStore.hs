{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Hash addressed store in file system.
--
-- Associates a key ('Control.FunFlow.ContentHashable.ContentHash')
-- with an item in the store. An item can either be
-- 'Control.FunFlow.ContentStore.Missing',
-- 'Control.FunFlow.ContentStore.Pending', or
-- 'Control.FunFlow.ContentStore.Complete'.
-- The state is persisted in the file system.
--
-- Items are stored under a path derived from their hash. Therefore,
-- there can be no two copies of the same item in the store.
-- If two keys are associated with the same item, then there will be
-- only one copy of that item in the store.
--
-- The store is thread-safe and multi-process safe.
--
-- It is assumed that the user that the process is running under is the owner
-- of the store root, or has permission to create it if missing.
--
-- It is assumed that the store root and its immediate contents are not modified
-- externally. The contents of pending items may be modified externally.
--
-- __Implementation notes:__
--
-- The hash of an item can only be determined once it is completed.
-- If that hash already exists in the store, then the new item is discarded.
--
-- Store state is persisted in the file-system:
--
-- * Pending items are stored writable under the path @pending-\<key>@.
-- * Complete items are stored read-only under the path @item-\<hash>@,
--   with a link under @complete-\<key>@ pointing to that directory.
module Control.FunFlow.ContentStore
  (
  -- * Open/Close
    withStore
  , open
  , close

  -- * List Contents
  , listAll
  , listPending
  , listComplete
  , listItems

  -- * Query/Lookup
  , query
  , isMissing
  , isPending
  , isComplete
  , lookup
  , lookupOrWait
  , waitUntilComplete

  -- * Construct Items
  , constructOrAsync
  , constructOrWait
  , constructIfMissing
  , markPending
  , markComplete

  -- * Remove Contents
  , removeFailed
  , removeForcibly
  , removeItemForcibly

  -- * Aliases
  , assignAlias
  , lookupAlias
  , removeAlias
  , listAliases

  -- * Accessors
  , buildPath
  , itemHash
  , itemPath
  , contentPath
  , contentItem
  , contentFilename
  , root

  -- * Types
  , ContentStore
  , Item
  , Content (..)
  , (^</>)
  , Alias (..)
  , Status (..)
  , Status_
  , Update (..)
  , StoreError (..)
  ) where


import           Prelude                             hiding (lookup)

import           Control.Arrow                       (second)
import           Control.Concurrent                  (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception                   (Exception, bracket_,
                                                      throwIO)
import           Control.FunFlow.ContentStore.Notify
import           Control.FunFlow.Orphans             ()
import           Control.Lens
import           Control.Monad                       (forever, void, (<=<),
                                                      (>=>))
import           Control.Monad.Catch                 (MonadMask, bracket)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Crypto.Hash                         (hashUpdate)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Bits                           (complement)
import qualified Data.ByteString.Char8               as C8
import           Data.Foldable                       (asum)
import           Data.List                           (foldl', stripPrefix)
import           Data.Maybe                          (fromMaybe, listToMaybe)
import           Data.Monoid                         ((<>))
import qualified Data.Store
import           Data.String                         (IsString)
import qualified Data.Text                           as T
import           Data.Typeable                       (Typeable)
import           Data.Void
import qualified Database.SQLite.Simple              as SQL
import qualified Database.SQLite.Simple.FromField    as SQL
import qualified Database.SQLite.Simple.ToField      as SQL
import           GHC.Generics                        (Generic)
import           Path
import           Path.IO
import           System.Directory                    (removePathForcibly)
import           System.FilePath                     (dropTrailingPathSeparator)
import           System.Posix.Files
import           System.Posix.Types

import           Control.FunFlow.ContentHashable     (ContentHash,
                                                      ContentHashable (..),
                                                      DirectoryContent (..),
                                                      contentHashUpdate_fingerprint,
                                                      encodeHash, pathToHash,
                                                      toBytes)
import           Control.FunFlow.Lock


-- | Status of an item in the store.
data Status missing pending complete
  = Missing missing
  -- ^ The item does not exist, yet.
  | Pending pending
  -- ^ The item is under construction and not ready for consumption.
  | Complete complete
  -- ^ The item is complete and ready for consumption.
  deriving (Eq, Show)

type Status_ = Status () () ()

-- | Update about the status of a pending item.
data Update
  = Completed Item
  -- ^ The item is now completed and ready for consumption.
  | Failed
  -- ^ Constructing the item failed.
  deriving (Eq, Show)

-- | Errors that can occur when interacting with the store.
data StoreError
  = NotPending ContentHash
  -- ^ An item is not under construction when it should be.
  | AlreadyPending ContentHash
  -- ^ An item is already under construction when it should be missing.
  | AlreadyComplete ContentHash
  -- ^ An item is already complete when it shouldn't be.
  | CorruptedLink ContentHash FilePath
  -- ^ The link under the given hash points to an invalid path.
  | FailedToConstruct ContentHash
  -- ^ A failure occurred while waiting for the item to be constructed.
  deriving (Show, Typeable)
instance Exception StoreError

-- | A hash addressed store on the file system.
data ContentStore = ContentStore
  { storeRoot     :: Path Abs Dir
  -- ^ Root directory of the content store.
  -- The process must be able to create this directory if missing,
  -- change permissions, and create files and directories within.
  , storeLock     :: Lock
  -- ^ Write lock on store metadata to ensure multi thread and process safety.
  -- The lock is taken when item state is changed or queried.
  , storeNotifier :: Notifier
  -- ^ Used to watch for updates on store items.
  , storeDb       :: SQL.Connection
  -- ^ Connection to the metadata SQLite database.
  }

-- | A completed item in the 'ContentStore'.
data Item = Item { itemHash :: ContentHash }
  deriving (Eq, Ord, Show, Generic)

instance Monad m => ContentHashable m Item where
  contentHashUpdate ctx item =
    flip contentHashUpdate_fingerprint item
    >=> pure . flip hashUpdate (toBytes $ itemHash item)
    $ ctx

instance FromJSON Item
instance ToJSON Item
instance Data.Store.Store Item

-- | File or directory within a content store 'Item'.
data Content t where
  All :: Item -> Content Dir
  (:</>) :: Item -> Path Rel t -> Content t
infixr 5 :</>
deriving instance Eq (Content t)
deriving instance Show (Content t)
instance Monad m => ContentHashable m (Content Dir) where
  contentHashUpdate ctx x = case x of
    All i ->
      flip contentHashUpdate_fingerprint x
      >=> flip contentHashUpdate i
      $ ctx
    i :</> p ->
      flip contentHashUpdate_fingerprint x
      >=> flip contentHashUpdate i
      >=> flip contentHashUpdate p
      $ ctx
instance Monad m => ContentHashable m (Content File) where
  contentHashUpdate ctx x = case x of
    i :</> p ->
      flip contentHashUpdate_fingerprint x
      >=> flip contentHashUpdate i
      >=> flip contentHashUpdate p
      $ ctx

-- | Append to the path within a store item.
(^</>) :: Content Dir -> Path Rel t -> Content t
All item ^</> path = item :</> path
(item :</> dir) ^</> path = item :</> dir </> path
infixl 4 ^</>

newtype Alias = Alias { unAlias :: T.Text }
  deriving (ContentHashable IO, Eq, Ord, Show, SQL.FromField, SQL.ToField, Data.Store.Store)

-- | The root directory of the store.
root :: ContentStore -> Path Abs Dir
root = storeRoot

-- | Path of the build directory of a pending item.
--
-- Beware, this does not check whether the item is actually pending.
buildPath :: ContentStore -> ContentHash -> Path Abs Dir
buildPath = mkPendingPath

-- | The store path of a completed item.
itemPath :: ContentStore -> Item -> Path Abs Dir
itemPath store = mkItemPath store . itemHash

-- | Store item containing the given content.
contentItem :: Content t -> Item
contentItem (All i)    = i
contentItem (i :</> _) = i

contentFilename :: Content File -> Path Rel File
contentFilename (_ :</> relPath) = filename relPath

-- | The absolute path to content within the store.
contentPath :: ContentStore -> Content t -> Path Abs t
contentPath store (All item)      = itemPath store item
contentPath store (item :</> dir) = itemPath store item </> dir

-- | @open root@ opens a store under the given root directory.
--
-- The root directory is created if necessary.
--
-- It is not safe to have multiple store objects
-- refer to the same root directory.
open :: Path Abs Dir -> IO ContentStore
open storeRoot = do
  createDirIfMissing True storeRoot
  storeLock <- openLock (lockPath storeRoot)
  withLock storeLock $ do
    storeDb <- SQL.open (fromAbsFile $ dbPath storeRoot)
    SQL.execute_ storeDb
      "CREATE TABLE IF NOT EXISTS\
      \  aliases\
      \  ( hash TEXT PRIMARY KEY\
      \  , dest TEXT NOT NULL\
      \  , name TEXT NOT NULL\
      \  )"
    setFileMode (fromAbsDir storeRoot) readOnlyRootDirMode
    storeNotifier <- initNotifier
    return ContentStore {..}

-- | Free the resources associated with the given store object.
--
-- The store object may not be used afterwards.
close :: ContentStore -> IO ()
close store = do
  closeLock (storeLock store)
  killNotifier (storeNotifier store)
  SQL.close (storeDb store)

-- | Open the store under the given root and perform the given action.
-- Closes the store once the action is complete
--
-- See also: 'Control.FunFlow.ContentStore.open'
withStore :: (MonadIO m, MonadMask m)
  => Path Abs Dir -> (ContentStore -> m a) -> m a
withStore root' = bracket (liftIO $ open root') (liftIO . close)

-- | List all elements in the store
-- @(pending keys, completed keys, completed items)@.
listAll :: ContentStore -> IO ([ContentHash], [ContentHash], [Item])
listAll ContentStore {storeRoot} =
  foldr go ([], [], []) . fst <$> listDir storeRoot
  where
    go d prev@(builds, outs, items) = fromMaybe prev $ asum
      [ parsePending d >>= \x -> Just (x:builds, outs, items)
      , parseComplete d >>= \x -> Just (builds, x:outs, items)
      , parseItem d >>= \x -> Just (builds, outs, x:items)
      ]
    parsePending :: Path Abs Dir -> Maybe ContentHash
    parsePending = pathToHash <=< stripPrefix pendingPrefix . extractDir
    parseComplete :: Path Abs Dir -> Maybe ContentHash
    parseComplete = pathToHash <=< stripPrefix completePrefix . extractDir
    parseItem :: Path Abs Dir -> Maybe Item
    parseItem = fmap Item . pathToHash <=< stripPrefix itemPrefix . extractDir
    extractDir :: Path Abs Dir -> FilePath
    extractDir = dropTrailingPathSeparator . fromRelDir . dirname

-- | List all pending keys in the store.
listPending :: ContentStore -> IO [ContentHash]
listPending = fmap (^._1) . listAll

-- | List all completed keys in the store.
listComplete :: ContentStore -> IO [ContentHash]
listComplete = fmap (^._2) . listAll

-- | List all completed items in the store.
listItems :: ContentStore -> IO [Item]
listItems = fmap (^._3) . listAll

-- | Query the state of the item under the given key.
query :: ContentStore -> ContentHash -> IO (Status () () ())
query store hash = withStoreLock store $
  internalQuery store hash >>= pure . \case
    Missing _ -> Missing ()
    Pending _ -> Pending ()
    Complete _ -> Complete ()

-- | Check if there is no complete or pending item under the given key.
isMissing :: ContentStore -> ContentHash -> IO Bool
isMissing store hash = (== Missing ()) <$> query store hash

-- | Check if there is a pending item under the given key.
isPending :: ContentStore -> ContentHash -> IO Bool
isPending store hash = (== Pending ()) <$> query store hash

-- | Check if there is a completed item under the given key.
isComplete :: ContentStore -> ContentHash -> IO Bool
isComplete store hash = (== Complete ()) <$> query store hash

-- | Query the state under the given key and return the item if completed.
-- Doesn't block if the item is pending.
lookup :: ContentStore -> ContentHash -> IO (Status () () Item)
lookup store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Missing () -> return $ Missing ()
    Pending _ -> return $ Pending ()
    Complete item -> return $ Complete item

-- | Query the state under the given key and return the item if completed.
-- Return an 'Control.Concurrent.Async' to await an update, if pending.
lookupOrWait
  :: ContentStore
  -> ContentHash
  -> IO (Status () (Async Update) Item)
lookupOrWait store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Complete item -> return $ Complete item
    Missing () -> return $ Missing ()
    Pending _ -> Pending <$> internalWatchPending store hash

-- | Query the state under the given key and return the item once completed.
-- Blocks if the item is pending.
-- Returns 'Nothing' if the item is missing, or failed to be completed.
waitUntilComplete :: ContentStore -> ContentHash -> IO (Maybe Item)
waitUntilComplete store hash = lookupOrWait store hash >>= \case
  Complete item -> return $ Just item
  Missing () -> return Nothing
  Pending a -> wait a >>= \case
    Completed item -> return $ Just item
    Failed -> return $ Nothing

-- | Atomically query the state under the given key and mark pending if missing.
--
-- Returns @'Complete' item@ if the item is complete.
-- Returns @'Pending' async@ if the item is pending, where @async@ is an
-- 'Control.Concurrent.Async' to await updates on.
-- Returns @'Missing' buildDir@ if the item was missing, and is now pending.
-- It should be constructed in the given @buildDir@,
-- and then marked as complete using 'markComplete'.
constructOrAsync
  :: ContentStore
  -> ContentHash
  -> IO (Status (Path Abs Dir) (Async Update) Item)
constructOrAsync store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Complete item -> return $ Complete item
    Missing () -> withWritableStore store $
      Missing <$> createBuildDir store hash
    Pending _ -> Pending <$> internalWatchPending store hash

-- | Atomically query the state under the given key and mark pending if missing.
-- Wait for the item to be completed, if already pending.
-- Throws a 'FailedToConstruct' error if construction fails.
--
-- Returns @'Complete' item@ if the item is complete.
-- Returns @'Missing' buildDir@ if the item was missing, and is now pending.
-- It should be constructed in the given @buildDir@,
-- and then marked as complete using 'markComplete'.
constructOrWait
  :: ContentStore
  -> ContentHash
  -> IO (Status (Path Abs Dir) Void Item)
constructOrWait store hash = constructOrAsync store hash >>= \case
  Pending a -> wait a >>= \case
    Completed item -> return $ Complete item
    -- XXX: Consider extending 'Status' with a 'Failed' constructor.
    --   If the store contains metadata as well, it could keep track of the
    --   number of failed attempts and further details about the failure.
    --   If an external task is responsible for the failure, the client could
    --   choose to resubmit a certain number of times.
    Failed -> throwIO $ FailedToConstruct hash
  Complete item -> return $ Complete item
  Missing dir -> return $ Missing dir

-- | Atomically query the state under the given key and mark pending if missing.
constructIfMissing
  :: ContentStore
  -> ContentHash
  -> IO (Status (Path Abs Dir) () Item)
constructIfMissing store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Complete item -> return $ Complete item
    Pending _ -> return $ Pending ()
    Missing () -> withWritableStore store $
      Missing <$> createBuildDir store hash

-- | Mark a non-existent item as pending.
--
-- Creates the build directory and returns its path.
--
-- See also: 'Control.FunFlow.ContentStore.constructIfMissing'.
markPending :: ContentStore -> ContentHash -> IO (Path Abs Dir)
markPending store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Complete _ -> throwIO (AlreadyComplete hash)
    Pending _ -> throwIO (AlreadyPending hash)
    Missing () -> withWritableStore store $
      createBuildDir store hash

-- | Mark a pending item as complete.
markComplete :: ContentStore -> ContentHash -> IO Item
markComplete store inHash = withStoreLock store $
  internalQuery store inHash >>= \case
    Missing () -> throwIO (NotPending inHash)
    Complete _ -> throwIO (AlreadyComplete inHash)
    Pending build -> withWritableStore store $ do
      unsetWritableRecursively build
      -- XXX: Hashing large data can take some time,
      --   could we avoid locking the store for all that time?
      outHash <- contentHash (DirectoryContent build)
      let out = mkItemPath store outHash
          link' = mkCompletePath store inHash
      doesDirExist out >>= \case
        True -> removePathForcibly (fromAbsDir build)
        False -> renameDir build out
      rel <- makeRelative (parent link') out
      let from' = dropTrailingPathSeparator $ fromAbsDir link'
          to' = dropTrailingPathSeparator $ fromRelDir rel
      createSymbolicLink to' from'
      pure $! Item outHash

-- | Remove a pending item.
--
-- It is the callers responsibility to ensure that no other threads or processes
-- will attempt to access the item's contents afterwards.
removeFailed :: ContentStore -> ContentHash -> IO ()
removeFailed store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Missing () -> throwIO (NotPending hash)
    Complete _ -> throwIO (AlreadyComplete hash)
    Pending build -> withWritableStore store $
      removePathForcibly (fromAbsDir build)

-- | Remove a key association independent of the corresponding item state.
-- Do nothing if no item exists under the given key.
--
-- It is the callers responsibility to ensure that no other threads or processes
-- will attempt to access the contents afterwards.
--
-- Note, this will leave an orphan item behind if no other keys point to it.
-- There is no garbage collection mechanism in place at the moment.
removeForcibly :: ContentStore -> ContentHash -> IO ()
removeForcibly store hash = withStoreLock store $ withWritableStore store $
  internalQuery store hash >>= \case
    Missing () -> pure ()
    Pending build -> removePathForcibly (fromAbsDir build)
    Complete _out ->
      removePathForcibly $
        dropTrailingPathSeparator $ fromAbsDir $ mkCompletePath store hash
      -- XXX: This will leave orphan store items behind.
      --   Add GC in some form.

-- | Remove a completed item in the store.
-- Do nothing if not completed.
--
-- It is the callers responsibility to ensure that no other threads or processes
-- will attempt to access the contents afterwards.
--
-- Note, this will leave keys pointing to that item dangling.
-- There is no garbage collection mechanism in place at the moment.
removeItemForcibly :: ContentStore -> Item -> IO ()
removeItemForcibly store item = withStoreLock store $ withWritableStore store $
  removePathForcibly (fromAbsDir $ itemPath store item)
  -- XXX: Remove dangling links.
  --   Add back-references in some form.

-- | Link the given alias to the given item.
-- If the alias existed before it is overwritten.
assignAlias :: ContentStore -> Alias -> Item -> IO ()
assignAlias store alias item =
  withStoreLock store $ withWritableStore store $ do
    hash <- contentHash alias
    SQL.executeNamed (storeDb store)
      "INSERT OR REPLACE INTO\
      \  aliases\
      \ VALUES\
      \  (:hash, :dest, :name)"
      [ ":hash" SQL.:= hash
      , ":dest" SQL.:= itemHash item
      , ":name" SQL.:= alias
      ]

-- | Lookup an item under the given alias.
-- Returns 'Nothing' if the alias does not exist.
lookupAlias :: ContentStore -> Alias -> IO (Maybe Item)
lookupAlias store alias =
  withStoreLock store $ do
    hash <- contentHash alias
    r <- SQL.queryNamed (storeDb store)
      "SELECT dest FROM aliases\
      \ WHERE\
      \  hash = :hash"
      [ ":hash" SQL.:= hash ]
    pure $! listToMaybe $ Item . SQL.fromOnly <$> r

-- | Remove the given alias.
removeAlias :: ContentStore -> Alias -> IO ()
removeAlias store alias =
  withStoreLock store $ withWritableStore store $ do
    hash <- contentHash alias
    SQL.executeNamed (storeDb store)
      "DELETE FROM aliases\
      \ WHERE\
      \  hash = :hash"
      [ ":hash" SQL.:= hash ]

-- | List all aliases and the respective items.
listAliases :: ContentStore -> IO [(Alias, Item)]
listAliases store = withStoreLock store $
  fmap (map (second Item)) $
    SQL.query_ (storeDb store)
      "SELECT name, dest FROM aliases"

----------------------------------------------------------------------
-- Internals

lockPath :: Path Abs Dir -> Path Abs File
lockPath = (</> [relfile|lock|])

dbPath :: Path Abs Dir -> Path Abs File
dbPath = (</> [relfile|metadata.db|])

-- | Holds a lock on the global 'MVar' and on the global lock file
-- for the duration of the given action.
withStoreLock :: ContentStore -> IO a -> IO a
withStoreLock store = withLock (storeLock store)

prefixHashPath :: C8.ByteString -> ContentHash -> Path Rel Dir
prefixHashPath pref hash
  | Just dir <- Path.parseRelDir $ C8.unpack $ pref <> encodeHash hash
  = dir
  | otherwise = error
      "[Control.FunFlow.ContentStore.prefixHashPath] \
      \Failed to construct hash path."

pendingPrefix, completePrefix, itemPrefix :: IsString s => s
pendingPrefix = "pending-"
completePrefix = "complete-"
itemPrefix = "item-"

-- | Return the full build path for the given input hash.
mkPendingPath :: ContentStore -> ContentHash -> Path Abs Dir
mkPendingPath ContentStore {storeRoot} hash =
  storeRoot </> prefixHashPath pendingPrefix hash

-- | Return the full link path for the given input hash.
mkCompletePath :: ContentStore -> ContentHash -> Path Abs Dir
mkCompletePath ContentStore {storeRoot} hash =
  storeRoot </> prefixHashPath completePrefix hash

-- | Return the full store path to the given output hash.
mkItemPath :: ContentStore -> ContentHash -> Path Abs Dir
mkItemPath ContentStore {storeRoot} hash =
  storeRoot </> prefixHashPath itemPrefix hash

-- | Query the state under the given key without taking a lock.
internalQuery
  :: ContentStore
  -> ContentHash
  -> IO (Status () (Path Abs Dir) Item)
internalQuery store inHash = do
  let build = mkPendingPath store inHash
      link' = mkCompletePath store inHash
  buildExists <- doesDirExist build
  if buildExists then
    pure $! Pending build
  else do
    linkExists <- doesDirExist link'
    if linkExists then do
      out <- readSymbolicLink
        (dropTrailingPathSeparator $ fromAbsDir link')
      case pathToHash =<< stripPrefix itemPrefix out of
        Nothing      -> throwIO $ CorruptedLink inHash out
        Just outHash -> return $ Complete (Item outHash)
    else
      pure $! Missing ()

-- | Create the build directory for the given input hash.
createBuildDir :: ContentStore -> ContentHash -> IO (Path Abs Dir)
createBuildDir store hash = do
  let dir = mkPendingPath store hash
  createDir dir
  setDirWritable dir
  return dir

-- | Watch the build directory of the pending item under the given key.
-- The returned 'Async' completes after the item is completed or failed.
internalWatchPending
  :: ContentStore
  -> ContentHash
  -> IO (Async Update)
internalWatchPending store hash = do
  let build = mkPendingPath store hash
  -- Add an inotify/kqueue watch and give a signal on relevant events.
  let notifier = storeNotifier store
  signal <- newEmptyMVar
  -- Signal the listener. If the 'MVar' is full,
  -- the listener didn't handle earlier signals, yet.
  let giveSignal = void $ tryPutMVar signal ()
  watch <- addDirWatch notifier (fromAbsDir build) giveSignal
  -- Additionally, poll on regular intervals.
  -- Inotify/Kqueue don't cover all cases, e.g. network filesystems.
  ticker <- async $ forever $ threadDelay 3007000 >> giveSignal
  let stopWatching = do
        cancel ticker
        removeDirWatch watch
  -- Listen to the signal asynchronously,
  -- and query the status when it fires.
  -- If the status changed, fill in the update.
  update <- newEmptyMVar
  let query' = withStoreLock store $ internalQuery store hash
      loop = takeMVar signal >> query' >>= \case
        Pending _ -> loop
        Complete item -> tryPutMVar update $ Completed item
        Missing () -> tryPutMVar update Failed
  void $ async loop
  -- Wait for the update asynchronously.
  -- Stop watching when it arrives.
  async $ takeMVar update <* stopWatching

setRootDirWritable :: ContentStore -> IO ()
setRootDirWritable ContentStore {storeRoot} =
  setFileMode (fromAbsDir storeRoot) writableRootDirMode

writableRootDirMode :: FileMode
writableRootDirMode = writableDirMode

setRootDirReadOnly :: ContentStore -> IO ()
setRootDirReadOnly ContentStore {storeRoot} =
  setFileMode (fromAbsDir storeRoot) readOnlyRootDirMode

readOnlyRootDirMode :: FileMode
readOnlyRootDirMode = writableDirMode `intersectFileModes` allButWritableMode

withWritableStore :: ContentStore -> IO a -> IO a
withWritableStore store =
  bracket_ (setRootDirWritable store) (setRootDirReadOnly store)

setDirWritable :: Path Abs Dir -> IO ()
setDirWritable fp = setFileMode (fromAbsDir fp) writableDirMode

writableDirMode :: FileMode
writableDirMode = foldl' unionFileModes nullFileMode
  [ directoryMode, ownerModes
  , groupReadMode, groupExecuteMode
  , otherReadMode, otherExecuteMode
  ]

-- | Unset write permissions on the given path.
unsetWritable :: Path Abs t -> IO ()
unsetWritable fp = do
  mode <- fileMode <$> getFileStatus (toFilePath fp)
  setFileMode (toFilePath fp) $ mode `intersectFileModes` allButWritableMode

allButWritableMode :: FileMode
allButWritableMode = complement $ foldl' unionFileModes nullFileMode
  [ownerWriteMode, groupWriteMode, otherWriteMode]

-- | Unset write permissions on all items in a directory tree recursively.
unsetWritableRecursively :: Path Abs Dir -> IO ()
unsetWritableRecursively = walkDir $ \dir _ files -> do
  mapM_ unsetWritable files
  unsetWritable dir
  return $ WalkExclude []
