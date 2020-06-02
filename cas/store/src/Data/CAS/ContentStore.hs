{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
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
-- Associates a key ('Data.CAS.ContentHashable.ContentHash')
-- with an item in the store. An item can either be
-- 'Data.CAS.ContentStore.Missing',
-- 'Data.CAS.ContentStore.Pending', or
-- 'Data.CAS.ContentStore.Complete'.
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
module Data.CAS.ContentStore
  (
  -- * Open/Close
    withStore
  , open
  , close

  -- * High-level API
  , CacherM (..)
  , Cacher
  , defaultCacherWithIdent
  , defaultIOCacherWithIdent
  , cacheKleisliIO
  , putInStore
  , contentPath

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
  , cacheComputation
  , constructIfMissing
  , withConstructIfMissing
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

  -- * Metadata
  , getBackReferences
  , setInputs
  , getInputs
  , setMetadata
  , getMetadata
  , createMetadataFile
  , getMetadataFile

  -- * Accessors
  , itemHash
  , itemPath
  , itemRelPath
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
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad                       (forM_, forever, unless,
                                                      void, when, (<=<), (>=>),
                                                      mzero)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Crypto.Hash                         (hashUpdate)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Bits                           (complement)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Char8               as C8
import           Data.CAS.ContentStore.Notify
import           Data.CAS.StoreOrphans               ()
import           Data.Foldable                       (asum)
import qualified Data.Hashable
import           Data.List                           (foldl', stripPrefix)
import           Data.Maybe                          (fromMaybe, listToMaybe)
import qualified Data.Store
import           Data.String                         (IsString (..))
import qualified Data.Text                           as T
import           Data.Void
import qualified Database.SQLite.Simple              as SQL
import qualified Database.SQLite.Simple.FromField    as SQL
import qualified Database.SQLite.Simple.ToField      as SQL
import           GHC.Generics                        (Generic)
import           Path
import           Path.IO
import           System.Directory                    (removePathForcibly)
import           System.FilePath                     (dropTrailingPathSeparator)
import           System.IO                           (Handle, IOMode (..),
                                                      openFile)
import           System.Posix.Files
import           System.Posix.Types

import           Data.CAS.ContentHashable            (ContentHash,
                                                      ContentHashable (..),
                                                      DirectoryContent (..),
                                                      contentHashUpdate_fingerprint,
                                                      decodeHash, encodeHash, pathToHash,
                                                      toBytes)
import           Data.CAS.Lock
import qualified Data.CAS.RemoteCache                as Remote
import           UnliftIO                            (MonadUnliftIO)


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
  | IncompatibleStoreVersion (Path Abs Dir) Int Int
  -- ^ @IncompatibleStoreVersion storeDir actual expected@
  --   The given store has a version number that is incompatible.
  | MalformedMetadataEntry ContentHash SQL.SQLData
  -- ^ @MalformedMetadataEntry hash key@
  --   The metadata entry for the give @hash@, @key@ pair is malformed.
  deriving (Show, Typeable)
instance Exception StoreError where
  displayException = \case
    NotPending hash ->
      "The following input hash is not pending '"
      ++ C8.unpack (encodeHash hash)
      ++ "'."
    AlreadyPending hash ->
      "The following input hash is already pending '"
      ++ C8.unpack (encodeHash hash)
      ++ "'."
    AlreadyComplete hash ->
      "The following input hash is already completed '"
      ++ C8.unpack (encodeHash hash)
      ++ "'."
    CorruptedLink hash fp ->
      "The completed input hash '"
      ++ C8.unpack (encodeHash hash)
      ++ "' points to an invalid store item '"
      ++ fp
      ++ "'."
    FailedToConstruct hash ->
      "Failed to construct the input hash '"
      ++ C8.unpack (encodeHash hash)
      ++ "'."
    IncompatibleStoreVersion storeDir actual expected ->
      "The store in '"
      ++ fromAbsDir storeDir
      ++ "' has version "
      ++ show actual
      ++ ". This software expects version "
      ++ show expected
      ++ ". No automatic migration is available, \
         \please use a fresh store location."
    MalformedMetadataEntry hash key ->
      "The metadtaa entry for hash '"
      ++ C8.unpack (encodeHash hash)
      ++ "' under key '"
      ++ show key
      ++ "' is malformed."

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
instance Data.Hashable.Hashable Item
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

-- | The scoped path to a content item within the store.
itemRelPath :: Item -> Path Rel Dir
itemRelPath (Item x) = prefixHashPath itemPrefix x

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
  withLock storeLock $ withWritableStoreRoot storeRoot $ do
    storeDb <- SQL.open (fromAbsFile $ dbPath storeRoot)
    initDb storeRoot storeDb
    createDirIfMissing True (metadataPath storeRoot)
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
-- See also: 'Data.CAS.ContentStore.open'
withStore :: (MonadIO m, MonadMask m)
  => Path Abs Dir -> (ContentStore -> m a) -> m a
withStore root' = bracket (liftIO $ open root') (liftIO . close)

-- | List all elements in the store
-- @(pending keys, completed keys, completed items)@.
listAll :: MonadIO m => ContentStore -> m ([ContentHash], [ContentHash], [Item])
listAll ContentStore {storeRoot} = liftIO $
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
listPending :: MonadIO m => ContentStore -> m [ContentHash]
listPending = fmap (^._1) . listAll

-- | List all completed keys in the store.
listComplete :: MonadIO m => ContentStore -> m [ContentHash]
listComplete = fmap (^._2) . listAll

-- | List all completed items in the store.
listItems :: MonadIO m => ContentStore -> m [Item]
listItems = fmap (^._3) . listAll

-- | Query the state of the item under the given key.
query :: MonadIO m => ContentStore -> ContentHash -> m (Status () () ())
query store hash = liftIO . withStoreLock store $
  internalQuery store hash >>= pure . \case
    Missing _ -> Missing ()
    Pending _ -> Pending ()
    Complete _ -> Complete ()

-- | Check if there is no complete or pending item under the given key.
isMissing :: MonadIO m => ContentStore -> ContentHash -> m Bool
isMissing store hash = (== Missing ()) <$> query store hash

-- | Check if there is a pending item under the given key.
isPending :: MonadIO m => ContentStore -> ContentHash -> m Bool
isPending store hash = (== Pending ()) <$> query store hash

-- | Check if there is a completed item under the given key.
isComplete :: MonadIO m => ContentStore -> ContentHash -> m Bool
isComplete store hash = (== Complete ()) <$> query store hash

-- | Query the state under the given key and return the item if completed.
-- Doesn't block if the item is pending.
lookup :: MonadIO m => ContentStore -> ContentHash -> m (Status () () Item)
lookup store hash = liftIO . withStoreLock store $
  internalQuery store hash >>= \case
    Missing () -> return $ Missing ()
    Pending _ -> return $ Pending ()
    Complete item -> return $ Complete item

-- | Query the state under the given key and return the item if completed.
-- Return an 'Control.Concurrent.Async' to await an update, if pending.
lookupOrWait
  :: MonadIO m
  => ContentStore
  -> ContentHash
  -> m (Status () (Async Update) Item)
lookupOrWait store hash = liftIO . withStoreLock store $
  internalQuery store hash >>= \case
    Complete item -> return $ Complete item
    Missing () -> return $ Missing ()
    Pending _ -> Pending <$> internalWatchPending store hash

-- | Query the state under the given key and return the item once completed.
-- Blocks if the item is pending.
-- Returns 'Nothing' if the item is missing, or failed to be completed.
waitUntilComplete :: MonadIO m => ContentStore -> ContentHash -> m (Maybe Item)
waitUntilComplete store hash = lookupOrWait store hash >>= \case
  Complete item -> return $ Just item
  Missing () -> return Nothing
  Pending a -> liftIO (wait a) >>= \case
    Completed item -> return $ Just item
    Failed -> return Nothing

-- | Atomically query the state under the given key and mark pending if missing.
constructIfMissing
  :: (MonadIO m, MonadUnliftIO m, MonadMask m, Remote.Cacher m remoteCache)
  => ContentStore
  -> remoteCache
  -> ContentHash
  -> m (Status (Path Abs Dir) () Item)
constructIfMissing store remoteCacher hash = withStoreLock store $
  internalQuery store hash >>= \case
    Complete item -> return $ Complete item
    Missing () -> withWritableStore store $ do
      let destDir :: Path Abs Dir = mkItemPath store hash
      Remote.pull remoteCacher hash destDir >>= \case
        Remote.PullOK () -> return $ Complete (Item hash)
        Remote.NotInCache ->
          Missing <$> liftIO (internalMarkPending store hash)
        Remote.PullError _ ->  -- TODO: That error should not be silenced
          Missing <$> liftIO (internalMarkPending store hash)
    Pending _ -> return $ Pending ()

-- | Atomically query the state under the given key and mark pending if missing.
-- Execute the given function to construct the item, mark as complete on success
-- and remove on failure. Forcibly removes if an uncaught exception occurs
-- during item construction.
withConstructIfMissing
  :: (MonadIO m, MonadUnliftIO m, MonadMask m, Remote.Cacher m remoteCache)
  => ContentStore
  -> remoteCache
  -> m () -- ^ In case an exception occurs (to log something for instance)
  -> ContentHash
  -> (Path Abs Dir -> m (Either e a))
  -> m (Status e () (Maybe a, Item))
withConstructIfMissing store remoteCacher ifExc hash f =
  bracketOnError
    (constructIfMissing store remoteCacher hash)
    (\status -> do
        case status of
          Missing _ -> removeForcibly store hash
          _ -> return ()
        ifExc)
    (\case
      Pending () -> return (Pending ())
      Complete item -> return (Complete (Nothing, item))
      Missing fp -> f fp >>= \case
        Left e -> do
          removeFailed store hash
          return (Missing e)
        Right x -> do
          item <- markComplete store hash
          _ <- Remote.push remoteCacher (itemHash item) (Just hash) (itemPath store item)
          return (Complete (Just x, item)))

-- | Mark a non-existent item as pending.
--
-- Creates the build directory and returns its path.
--
-- See also: 'Data.CAS.ContentStore.constructIfMissing'.
markPending :: MonadIO m => ContentStore -> ContentHash -> m (Path Abs Dir)
markPending store hash = liftIO . withStoreLock store $
  internalQuery store hash >>= \case
    Complete _ -> throwIO (AlreadyComplete hash)
    Pending _ -> throwIO (AlreadyPending hash)
    Missing () -> withWritableStore store $
      internalMarkPending store hash

-- | Mark a pending item as complete.
markComplete :: MonadIO m => ContentStore -> ContentHash -> m Item
markComplete store inHash = liftIO . withStoreLock store $
  internalQuery store inHash >>= \case
    Missing () -> throwIO (NotPending inHash)
    Complete _ -> throwIO (AlreadyComplete inHash)
    Pending build -> withWritableStore store $ liftIO $ do
      do
        let metadataDir = mkMetadataDirPath store inHash
        exists <- doesDirExist metadataDir
        when exists $
          unsetWritableRecursively metadataDir
      -- XXX: Hashing large data can take some time,
      --   could we avoid locking the store for all that time?
      outHash <- contentHash (DirectoryContent build)
      let out = mkItemPath store outHash
          link' = mkCompletePath store inHash
      doesDirExist out >>= \case
        True -> removePathForcibly (fromAbsDir build)
        False -> do
          renameDir build out
          unsetWritableRecursively out
      rel <- makeRelative (parent link') out
      let from' = dropTrailingPathSeparator $ fromAbsDir link'
          to' = dropTrailingPathSeparator $ fromRelDir rel
      createSymbolicLink to' from'
      addBackReference store inHash (Item outHash)
      pure $! Item outHash

-- | Remove a pending item.
--
-- It is the callers responsibility to ensure that no other threads or processes
-- will attempt to access the item's contents afterwards.
removeFailed :: MonadIO m => ContentStore -> ContentHash -> m ()
removeFailed store hash = liftIO . withStoreLock store $
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
removeForcibly :: MonadIO m => ContentStore -> ContentHash -> m ()
removeForcibly store hash = liftIO . withStoreLock store $ withWritableStore store $
  internalQuery store hash >>= \case
    Missing () -> pure ()
    Pending build -> liftIO $ removePathForcibly (fromAbsDir build)
    Complete _out -> liftIO $
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
removeItemForcibly :: MonadIO m => ContentStore -> Item -> m ()
removeItemForcibly store item = liftIO . withStoreLock store $ withWritableStore store $
  removePathForcibly (fromAbsDir $ itemPath store item)
  -- XXX: Remove dangling links.
  --   Add back-references in some form.

-- We need this orphan instance here so cas-hash doesn't depend on sqlite
instance SQL.FromField ContentHash where
  fromField f = do
    bs <- SQL.fromField f
    case decodeHash bs of
      Just h  -> pure h
      Nothing -> mzero

instance SQL.ToField ContentHash where
  toField = SQL.toField . encodeHash

-- | Link the given alias to the given item.
-- If the alias existed before it is overwritten.
assignAlias :: MonadIO m => ContentStore -> Alias -> Item -> m ()
assignAlias store alias item =
  liftIO . withStoreLock store $ withWritableStore store $ do
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
lookupAlias :: MonadIO m => ContentStore -> Alias -> m (Maybe Item)
lookupAlias store alias =
  liftIO . withStoreLock store $ do
    hash <- contentHash alias
    r <- SQL.queryNamed (storeDb store)
      "SELECT dest FROM aliases\
      \ WHERE\
      \  hash = :hash"
      [ ":hash" SQL.:= hash ]
    pure $! listToMaybe $ Item . SQL.fromOnly <$> r

-- | Remove the given alias.
removeAlias :: MonadIO m => ContentStore -> Alias -> m ()
removeAlias store alias =
  liftIO . withStoreLock store $ withWritableStore store $ do
    hash <- contentHash alias
    SQL.executeNamed (storeDb store)
      "DELETE FROM aliases\
      \ WHERE\
      \  hash = :hash"
      [ ":hash" SQL.:= hash ]

-- | List all aliases and the respective items.
listAliases :: MonadIO m => ContentStore -> m [(Alias, Item)]
listAliases store = liftIO . withStoreLock store $
  fmap (map (second Item)) $
    SQL.query_ (storeDb store)
      "SELECT name, dest FROM aliases"

-- | Get all hashes that resulted in the given item.
getBackReferences :: MonadIO m => ContentStore -> Item -> m [ContentHash]
getBackReferences store (Item outHash) = liftIO . withStoreLock store $
  map SQL.fromOnly <$> SQL.queryNamed (storeDb store)
    "SELECT hash FROM backrefs\
    \ WHERE\
    \  dest = :out"
    [ ":out" SQL.:= outHash ]

-- | Define the input items to a subtree.
setInputs :: MonadIO m => ContentStore -> ContentHash -> [Item] -> m ()
setInputs store hash items = liftIO $
  withStoreLock store $
  withWritableStore store $
  internalQuery store hash >>= \case
    Pending _ -> forM_ items $ \(Item input) ->
      SQL.executeNamed (storeDb store)
        "INSERT OR REPLACE INTO\
        \  inputs (hash, input)\
        \ VALUES\
        \  (:hash, :input)"
        [ ":hash" SQL.:= hash
        , ":input" SQL.:= input
        ]
    _ -> throwIO $ NotPending hash

-- | Get the input items to a subtree if any were defined.
getInputs :: MonadIO m => ContentStore -> ContentHash -> m [Item]
getInputs store hash = liftIO . withStoreLock store $
  map (Item . SQL.fromOnly) <$> SQL.queryNamed (storeDb store)
    "SELECT input FROM inputs\
    \ WHERE\
    \  hash = :hash"
    [ ":hash" SQL.:= hash ]

-- | Set a metadata entry on an item.
setMetadata :: (SQL.ToField k, SQL.ToField v, MonadIO m )
            => ContentStore -> ContentHash -> k -> v -> m ()
setMetadata store hash k v = liftIO $
  withStoreLock store $
  withWritableStore store $
  SQL.executeNamed (storeDb store)
    "INSERT OR REPLACE INTO\
    \  metadata (hash, key, value)\
    \ VALUES\
    \  (:hash, :key, :value)"
    [ ":hash" SQL.:= hash
    , ":key" SQL.:= k
    , ":value" SQL.:= v
    ]

-- | Retrieve a metadata entry on an item, or 'Nothing' if missing.
getMetadata :: (SQL.ToField k, SQL.FromField v, MonadIO m)
  => ContentStore -> ContentHash -> k -> m (Maybe v)
getMetadata store hash k = liftIO . withStoreLock store $ do
  r <- SQL.queryNamed (storeDb store)
    "SELECT value FROM metadata\
    \ WHERE\
    \  (hash = :hash AND key = :key)"
    [ ":hash" SQL.:= hash
    , ":key" SQL.:= k
    ]
  case r of
    []    -> pure Nothing
    [[v]] -> pure $ Just v
    _     -> throwIO $ MalformedMetadataEntry hash (SQL.toField k)

-- | Create and open a new metadata file on a pending item in write mode.
createMetadataFile
  :: MonadIO m
  => ContentStore -> ContentHash -> Path Rel File -> m (Path Abs File, Handle)
createMetadataFile store hash file = liftIO . withStoreLock store $
  internalQuery store hash >>= \case
    Pending _ -> do
      let path = mkMetadataFilePath store hash file
      createDirIfMissing True (parent path)
      hndl <- openFile (fromAbsFile path) WriteMode
      pure (path, hndl)
    _ -> throwIO $ NotPending hash

-- | Return the path to a metadata file if it exists.
getMetadataFile
  :: MonadIO m
  => ContentStore -> ContentHash -> Path Rel File -> m (Maybe (Path Abs File))
getMetadataFile store hash file = liftIO . withStoreLock store $ do
  let path = mkMetadataFilePath store hash file
  exists <- doesFileExist path
  if exists then
    pure $ Just path
  else
    pure Nothing

----------------------------------------------------------------------
-- Internals

lockPath :: Path Abs Dir -> Path Abs Dir
lockPath = (</> [reldir|lock|])

dbPath :: Path Abs Dir -> Path Abs File
dbPath = (</> [relfile|metadata.db|])

metadataPath :: Path Abs Dir -> Path Abs Dir
metadataPath = (</> [reldir|metadata|])

-- | Holds a lock on the global 'MVar' and on the global lock file
-- for the duration of the given action.
withStoreLock :: MonadUnliftIO m => ContentStore -> m a -> m a
withStoreLock store = withLock (storeLock store)

prefixHashPath :: C8.ByteString -> ContentHash -> Path Rel Dir
prefixHashPath pref hash
  | Just dir <- Path.parseRelDir $ C8.unpack $ pref <> encodeHash hash
  = dir
  | otherwise = error
      "[Data.CAS.ContentStore.prefixHashPath] \
      \Failed to construct hash path."

pendingPrefix, completePrefix, hashPrefix, itemPrefix :: IsString s => s
pendingPrefix = "pending-"
completePrefix = "complete-"
hashPrefix = "hash-"
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

-- | Return the full store path to the given metadata directory.
mkMetadataDirPath :: ContentStore -> ContentHash -> Path Abs Dir
mkMetadataDirPath ContentStore {storeRoot} hash =
  metadataPath storeRoot </> prefixHashPath hashPrefix hash

-- | Return the full store path to the given metadata file.
mkMetadataFilePath
  :: ContentStore -> ContentHash -> Path Rel File -> Path Abs File
mkMetadataFilePath store hash file =
  mkMetadataDirPath store hash </> file

-- | Query the state under the given key without taking a lock.
internalQuery
  :: MonadIO m
  => ContentStore
  -> ContentHash
  -> m (Status () (Path Abs Dir) Item)
internalQuery store inHash = liftIO $ do
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

-- | Create the build directory for the given input hash
--   and make the metadata directory writable if it exists.
internalMarkPending :: ContentStore -> ContentHash -> IO (Path Abs Dir)
internalMarkPending store hash = do
  let dir = mkPendingPath store hash
  createDir dir
  setDirWritable dir
  let metadataDir = mkMetadataDirPath store hash
  metadirExists <- doesDirExist metadataDir
  when metadirExists $
    setWritableRecursively metadataDir
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
  let query' = liftIO . withStoreLock store $ internalQuery store hash
      loop = takeMVar signal >> query' >>= \case
        Pending _ -> loop
        Complete item -> tryPutMVar update $ Completed item
        Missing () -> tryPutMVar update Failed
  void $ async loop
  -- Wait for the update asynchronously.
  -- Stop watching when it arrives.
  async $ takeMVar update <* stopWatching

setRootDirWritable :: MonadIO m => Path Abs Dir -> m ()
setRootDirWritable storeRoot = liftIO $
  setFileMode (fromAbsDir storeRoot) writableRootDirMode

writableRootDirMode :: FileMode
writableRootDirMode = writableDirMode

setRootDirReadOnly :: MonadIO m => Path Abs Dir -> m ()
setRootDirReadOnly storeRoot = liftIO $
  setFileMode (fromAbsDir storeRoot) readOnlyRootDirMode

readOnlyRootDirMode :: FileMode
readOnlyRootDirMode = writableDirMode `intersectFileModes` allButWritableMode

withWritableStoreRoot :: (MonadMask m, MonadIO m) => Path Abs Dir -> m a -> m a
withWritableStoreRoot storeRoot =
  bracket_ (setRootDirWritable storeRoot) (setRootDirReadOnly storeRoot)

withWritableStore :: (MonadMask m, MonadIO m) => ContentStore -> m a -> m a
withWritableStore ContentStore {storeRoot} =
  withWritableStoreRoot storeRoot

setDirWritable :: Path Abs Dir -> IO ()
setDirWritable fp = setFileMode (fromAbsDir fp) writableDirMode

writableDirMode :: FileMode
writableDirMode = foldl' unionFileModes nullFileMode
  [ directoryMode, ownerModes
  , groupReadMode, groupExecuteMode
  , otherReadMode, otherExecuteMode
  ]

-- | Set write permissions on the given path.
setWritable :: Path Abs t -> IO ()
setWritable fp = do
  mode <- fileMode <$> getFileStatus (toFilePath fp)
  setFileMode (toFilePath fp) $ mode `unionFileModes` ownerWriteMode

-- | Unset write permissions on the given path.
unsetWritable :: Path Abs t -> IO ()
unsetWritable fp = do
  mode <- fileMode <$> getFileStatus (toFilePath fp)
  setFileMode (toFilePath fp) $ mode `intersectFileModes` allButWritableMode

allButWritableMode :: FileMode
allButWritableMode = complement $ foldl' unionFileModes nullFileMode
  [ownerWriteMode, groupWriteMode, otherWriteMode]

-- | Set write permissions on all items in a directory tree recursively.
setWritableRecursively :: Path Abs Dir -> IO ()
setWritableRecursively = walkDir $ \dir _ files -> do
  mapM_ setWritable files
  setWritable dir
  return $ WalkExclude []

-- | Unset write permissions on all items in a directory tree recursively.
unsetWritableRecursively :: Path Abs Dir -> IO ()
unsetWritableRecursively = walkDir $ \dir _ files -> do
  mapM_ unsetWritable files
  unsetWritable dir
  return $ WalkExclude []

storeVersion :: Int
storeVersion = 1

-- | Initialize the database.
initDb :: Path Abs Dir -> SQL.Connection -> IO ()
initDb storeDir db = do
  [[version]] <- SQL.query_ db "PRAGMA user_version"
  if version == 0 then
    SQL.execute_ db $
      "PRAGMA user_version = " <> fromString (show storeVersion)
  else
    unless (version == storeVersion) $
      throwIO $ IncompatibleStoreVersion storeDir version storeVersion
  -- Aliases to items.
  SQL.execute_ db
    "CREATE TABLE IF NOT EXISTS\
    \  aliases\
    \  ( hash TEXT PRIMARY KEY\
    \  , dest TEXT NOT NULL\
    \  , name TEXT NOT NULL\
    \  )"
  -- Back-references from items @dest@ to hashes @hash@.
  SQL.execute_ db
    "CREATE TABLE IF NOT EXISTS\
    \  backrefs\
    \  ( hash TEXT PRIMARY KEY\
    \  , dest TEXT NOT NULL\
    \  )"
  -- Inputs @input@ to hashes @hash@.
  SQL.execute_ db
    "CREATE TABLE IF NOT EXISTS\
    \  inputs\
    \  ( hash TEXT NOT NULL\
    \  , input TEXT NOT NULL\
    \  , UNIQUE (hash, input)\
    \  )"
  -- Arbitrary metadata on hashes.
  SQL.execute_ db
    "CREATE TABLE IF NOT EXISTS\
    \  metadata\
    \  ( hash  TEXT NOT NULL\
    \  , key   TEXT NOT NULL\
    \  , value TEXT\
    \  , PRIMARY KEY(hash, key)\
    \  )"

-- | Adds a link between input hash and the output hash.
--
-- Assumes that the store is locked and writable.
addBackReference :: ContentStore -> ContentHash -> Item -> IO ()
addBackReference store inHash (Item outHash) =
  SQL.executeNamed (storeDb store)
    "INSERT OR REPLACE INTO\
    \  backrefs (hash, dest)\
    \ VALUES\
    \  (:in, :out)"
    [ ":in" SQL.:= inHash
    , ":out" SQL.:= outHash
    ]

-- | A cacher is responsible for controlling how steps are cached.
data CacherM m i o =
    NoCache -- ^ This step cannot be cached (default).
  | Cache
    { -- | Function to encode the input into a content
      --   hash.
      --   This function additionally takes an
      --   'identities' which gets incorporated into
      --   the cacher.
      cacherKey        :: Int -> i -> m ContentHash
    , cacherStoreValue :: o -> ByteString
      -- | Attempt to read the cache value back. May throw exceptions.
    , cacherReadValue  :: ByteString -> o
    }

-- | A pure 'CacherM'
type Cacher = CacherM Identity

-- | Constructs a 'Cacher' that will use hashability of input and
-- serializability of output to make a step cacheable
defaultCacherWithIdent :: forall m i o.
                          (ContentHashable m i, Data.Store.Store o)
                       => Int -- ^ Seed for the cacher
                       -> CacherM m i o
defaultCacherWithIdent ident = Cache
  { cacherKey = \i ident' -> contentHash (ident', ident, i)
  , cacherStoreValue = Data.Store.encode
  , cacherReadValue = Data.Store.decodeEx
  }

-- | Looks for a @CacherM IO@, then lifts it
defaultIOCacherWithIdent :: (MonadIO m, ContentHashable IO i, Data.Store.Store o)
                         => Int -- ^ Seed for the cacher
                         -> CacherM m i o
defaultIOCacherWithIdent ident = c{cacherKey = \x i -> liftIO $ cacherKey c x i}
  where c = defaultCacherWithIdent ident

-- | Runs a computation only if the ContentHash isn't already associated to an
-- entry in the store
cacheComputation
  :: (MonadIO m, MonadUnliftIO m, MonadMask m, Remote.Cacher m remoteCache)
  => ContentStore
  -> remoteCache
  -> m ()                  -- ^ In case an exception occurs
  -> ContentHash           -- ^ A ContentHash to identify the computation inputs
  -> (Path Abs Dir -> m a) -- ^ The computation to cache, receving the path of a
                           -- store folder to which it should write its results
  -> m (Maybe a, Item)     -- ^ The result if it was just computed, and the item
                           -- corresponding to the store folder
cacheComputation store remoteCacher ifExc inputCHash computation =
  withConstructIfMissing store remoteCacher ifExc inputCHash (fmap Right . computation) >>= \case
    Missing e -> absurd e
    Pending _ ->
      liftIO (waitUntilComplete store inputCHash) >>= \case
        Just item -> return (Nothing, item)
        Nothing -> throwM $ FailedToConstruct inputCHash
    Complete resultAndItem -> return resultAndItem

-- | Caches a Kleisli of some MonadIO action in the store given the required
-- properties
cacheKleisliIO
  :: (MonadIO m, MonadUnliftIO m, MonadMask m, Remote.Cacher m remoteCache)
  => Maybe Int  -- ^ This can be used to disambiguate the same program run in
                -- multiple configurations. If Nothing, then it means this
                -- program has no identity, this implies that steps will be
                -- executed without cache, even if 'Cache' has been given.
  -> CacherM m i o
  -> ContentStore
  -> remoteCache
  -> (i -> m o)
  -> i
  -> m o
cacheKleisliIO confIdent c@Cache{} store remoteCacher f i
  | Just confIdent' <- confIdent = do
      chash <- cacherKey c confIdent' i
      (res, item) <- cacheComputation store remoteCacher (return ()) chash computeAndStore
      case res of
        Just r -> return r
        Nothing -> do
          bs <- liftIO . BS.readFile $ toFilePath $ itemPath store item </> [relfile|out|]
          return $ cacherReadValue c $ bs
  where computeAndStore fp = do
          res <- f i  -- Do the actual computation
          liftIO $ BS.writeFile (toFilePath $ fp </> [relfile|out|])
                 $ cacherStoreValue c $ res
          return res
cacheKleisliIO _ _ _ _ f i = f i

-- | Caches an action that writes content-addressed data to the store. Returns
-- the Item of the written content.
putInStore
  :: (MonadIO m, MonadMask m, MonadUnliftIO m
     ,Remote.Cacher m remoteCacher
     ,ContentHashable IO t)
  => ContentStore
  -> remoteCacher
  -> (ContentHash -> m ()) -- ^ In case an exception occurs
  -> (Path Abs Dir -> t -> m ()) -- ^ The action that writes to the new store
                                 -- directory
  -> t
  -> m Item -- ^ The Item in the store to which @t@ has been written
putInStore store remoteCacher ifExc f x = do
  chash <- liftIO $ contentHash x
  snd <$> cacheComputation store remoteCacher (ifExc chash) chash (flip f x)
