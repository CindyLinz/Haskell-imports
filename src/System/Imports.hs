module System.Imports where

import System.Directory
import System.FilePath
import System.IO

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Except

import Data.Monoid

type Predictor
  = FilePath -- ^ paths from search root: "Bar/Ex/Ex2/Foo.hs" or "Bar/Ex/Ex2/" (paths to directory are ended with "/" on Posix or "\\" on Windows)
  -> IO Bool

defaultPred :: Predictor
defaultPred "" = return False
defaultPred ('.':_) = return False
defaultPred ('_':_) = return False
defaultPred path
  | isPathSeparator $ last path = return True
  | takeExtension path == ".hs" = return True
  | takeExtension path == ".lhs" = return True
  | otherwise = return False

pathToModule :: FilePath -> String
pathToModule path = go $ dropExtensions path where
  go [] = []
  go (a:as)
    | isPathSeparator a = '.' : go as
    | otherwise = a : go as

searchImportsWith
  :: Predictor
  -> FilePath -- ^ path to the search root
  -> IO [String] -- ^ something like ["Foo", "Foo.Bar", "Foo.Bar2"], relative to the search root
searchImportsWith p path
  | let filename = takeFileName path, filename == "." || filename == ".."
    = return []
  | otherwise = execWriterT $ do
  do
    toDir <- (either (const $ return False) (const $ return True) =<<) . lift . runExceptT $ do
      isDir <- lift $ doesDirectoryExist path
      unless isDir (throwError ())

      wantDir <- lift $ p (path ++ "/")
      unless wantDir (throwError ())

      return ()

    when toDir $ do
      entries <- lift $ getDirectoryContents path
      forM_ entries $ \entry ->
        tell =<< lift (searchImportsWith p (path ++ [pathSeparator]))

  do
    gotFile <- (either (const $ return False) (const $ return True) =<<) . lift . runExceptT $ do
      isFile <- lift $ doesFileExist path
      unless isFile (throwError ())

      wantFile <- lift $ p path
      unless wantFile (throwError ())

      return ()

    when gotFile $ do
      tell [pathToModule path]

searchImports
  :: FilePath -- ^ path to the search root
  -> IO [String] -- ^ something like ["Foo", "Foo.Bar", "Foo.Bar2"], relative to the search root
searchImports = searchImportsWith defaultPred

importsContentWith
  :: Predictor
  -> String -- ^ import alias
  -> [(String, FilePath)] -- ^ [(prefix, search root)]
  -> IO String
importsContentWith p alias sources = execWriterT $ do
  forM_ sources $ \(prefix', root) -> do
    let prefix = if null prefix' then "" else prefix' ++ "."
    imports <- lift $ searchImportsWith p root
    forM_ imports $ \im -> do
      tell $ "import qualified " ++ prefix ++ im ++ " as " ++ alias ++ "\n"

importsContent
  :: String -- ^ import alias
  -> [(String, FilePath)] -- ^ [(prefix, search root)]
  -> IO String
importsContent = importsContentWith defaultPred

writeMultiImportsHeaderWith
  :: Predictor
  -> FilePath -- ^ import header file to write
  -> String -- ^ import alias
  -> [(String, FilePath)] -- ^ [(module name prefix, path to the search root)]
  -> IO ()
writeMultiImportsHeaderWith p headerPath alias sources = do
  headerContent <- importsContentWith p alias sources
  writeFile headerPath headerContent

writeMultiImportsHeader
  :: FilePath -- ^ import header file to write
  -> String -- ^ import alias
  -> [(String, FilePath)] -- ^ [(module name prefix, path to the search root)]
  -> IO ()
writeMultiImportsHeader = writeMultiImportsHeaderWith defaultPred

writeImportsHeaderWith
  :: Predictor
  -> FilePath -- ^ import header file to write
  -> String -- ^ import alias
  -> String -- ^ module name prefix
  -> FilePath -- ^ path to the search root
  -> IO ()
writeImportsHeaderWith p headerPath alias prefix rootPath = writeMultiImportsHeaderWith p headerPath alias [(prefix, rootPath)]

writeImportsHeader
  :: FilePath -- ^ import header file to write
  -> String -- ^ import alias
  -> String -- ^ module name prefix
  -> FilePath -- ^ path to the search root
  -> IO ()
writeImportsHeader = writeImportsHeaderWith defaultPred
