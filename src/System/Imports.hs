module System.Imports where

import System.Directory
import System.FilePath
import System.IO

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Except

import Data.Monoid
import Data.Either

type Predictor
  = FilePath -- ^ paths from search root: "Bar/Ex/Ex2/Foo.hs" or "Bar/Ex/Ex2" (you should determine the path is whether a file or a directory)
  -> IO Bool

defaultPred :: Predictor
defaultPred path =
  case takeFileName path of
    "" -> return False
    ('.':_) -> return False
    ('_':_) -> return False
    filename -> liftM isLeft . runExceptT $ do
      lift (doesDirectoryExist path) >>= flip when (throwError ())
      let ext = takeExtension filename
      when (ext == ".hs" || ext == ".lhs") (throwError ())
      return ()

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
searchImportsWith p rootPath = go "" where
  go subPath = execWriterT $ do
    let thisPath = rootPath </> subPath

    entries <- lift $ getDirectoryContents thisPath
    forM_ entries $ \entry -> do
      let entryPath = thisPath </> entry
      let subPath' = subPath </> entry

      toDo <- lift . liftM isRight . runExceptT $ do
        when (entry == "" || entry == "." || entry == "..") (throwError ())
        flip unless (throwError ()) =<< lift (p entryPath)
        return ()

      when toDo $ do
        (lift (doesDirectoryExist entryPath) >>=) . flip when $ do
          tell =<< lift (go subPath')
        (lift (doesFileExist entryPath) >>=) . flip when $ do
          tell [pathToModule subPath']

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
      tell $ "import " ++ prefix ++ im ++ " as " ++ alias ++ "\n"

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

writeMultiImportsModuleWith
  :: Predictor
  -> String -- ^ module name
  -> FilePath -- ^ module file to write
  -> [(String, FilePath)] -- ^ [(module name prefix, path to the search root)]
  -> IO ()
writeMultiImportsModuleWith p moduleName modulePath sources = do
  headerContent <- importsContentWith p "Export" sources
  writeFile modulePath $ "module " ++ moduleName ++ " (module Export) where\n" ++ headerContent

writeMultiImportsModule
  :: String -- ^ module name
  -> FilePath -- ^ module file to write
  -> [(String, FilePath)] -- ^ [(module name prefix, path to the search root)]
  -> IO ()
writeMultiImportsModule = writeMultiImportsModuleWith defaultPred

writeImportsModuleWith
  :: Predictor
  -> String -- ^ module name
  -> FilePath -- ^ module file to write
  -> String -- ^ module name prefix
  -> FilePath -- ^ path to the search root
  -> IO ()
writeImportsModuleWith p moduleName modulePath prefix rootPath =
  writeMultiImportsModuleWith p moduleName modulePath [(prefix, rootPath)]

writeImportsModule
  :: String -- ^ module name
  -> FilePath -- ^ module file to write
  -> String -- ^ module name prefix
  -> FilePath -- ^ path to the search root
  -> IO ()
writeImportsModule = writeImportsModuleWith defaultPred
