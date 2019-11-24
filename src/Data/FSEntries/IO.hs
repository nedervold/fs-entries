-- | Functions for reading and writing 'FSEntries' values.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.FSEntries.IO
  ( readFSEntries
  , writeFSEntries
    -- * IO to/from filesystem
  , readFSEntriesFromFS
  , writeFSEntriesToFS
    -- * utilities
  , drawDirectory
  ) where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.FSEntries.Forest (drawFSEntries)
import Data.FSEntries.Types
import qualified Data.Map as M
import System.Directory
  ( createDirectory
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , makeAbsolute
  )
import System.FilePath ((</>), makeRelative)
import Text.Printf (printf)

-- | Given functions to read directory and file data respectively,
-- return a function that reads an 'FSEntries' value.
readFSEntries ::
     forall d f m. MonadIO m
  => (FilePath -> m d)
  -> (FilePath -> m f)
  -> FilePath
  -> m (FSEntries d f)
readFSEntries readDirData readFileData rootDir = do
  exists <- liftIO $ doesDirectoryExist rootDir
  unless exists $
    error $ printf "readFSEntries called on non-directory %s" rootDir
  readEntries ""
  where
    readEntries :: FilePath -> m (FSEntries d f)
    readEntries dir = do
      entries <- liftIO $ listDirectory (rootDir </> dir)
      mkFSEntries <$>
        sequence
          [(entry, ) <$> readEntry (dir </> entry) | entry <- entries]
    readEntry :: FilePath -> m (FSEntry d f)
    readEntry path = do
      isDir <- liftIO $ doesDirectoryExist (rootDir </> path)
      if isDir
        then readDir path
        else do
          isFile <- liftIO $ doesFileExist (rootDir </> path)
          if isFile
            then readFile' path
            else error $
                 printf
                   "readFSEntries: %s is neither directory nor file"
                   path
    readFile' :: FilePath -> m (FSEntry d f)
    readFile' fp = File <$> readFileData (rootDir </> fp)
    readDir :: FilePath -> m (FSEntry d f)
    readDir fp = Dir <$> readDirData (rootDir </> fp) <*> readEntries fp

-- | Read an 'FSEntries' value from the filesystem at the given path.
readFSEntriesFromFS :: FilePath -> IO (FSEntries () ByteString)
readFSEntriesFromFS = readFSEntries readDirData readFileData
  where
    readDirData _fp = pure ()
    readFileData = BS.readFile

-- | Given functions to write directory data (but not contents) and
-- file data respectively, return a function that writes an
-- 'FSEntries' value at a 'FilePath'.  Typically you will only create
-- the directory and possibly set metadata; the directory will be
-- filled recursively.
writeFSEntries ::
     forall d f m. MonadIO m
  => (FilePath -> d -> m ())
  -> (FilePath -> f -> m ())
  -> FilePath
  -> FSEntries d f
  -> m ()
writeFSEntries writeDir' writeFile' fp entries = do
  absFP <- liftIO $ makeAbsolute fp
  writeEntries' absFP entries
  where
    writeEntries' :: FilePath -> FSEntries d f -> m ()
    writeEntries' dir entries' =
      forM_ (M.toList $ unFSEntries entries') $ \(nm, entry) ->
        let dir' = dir </> nm
         in case entry of
              Dir d entries'' -> do
                writeDir' dir' d
                writeEntries' dir' entries''
              File f -> writeFile' dir' f

-- | Write an 'FSEntries' value to the filesystem at the given path.
writeFSEntriesToFS ::
     MonadIO m => FilePath -> FSEntries () ByteString -> m ()
writeFSEntriesToFS = writeFSEntries writeDir' writeFile'
  where
    writeDir' fp () = liftIO $ createDirectory fp
    writeFile' fp bs = liftIO $ BS.writeFile fp bs

-- | Utility function to draw a diagram of a directory and its contents.
drawDirectory :: FilePath -> IO ()
drawDirectory fp = do
  entries <-
    readFSEntries (pure . makeRelative fp) (pure . makeRelative fp) fp
  putStrLn $ drawFSEntries id id entries
