{- |
Module      : System.Imports
Description : Generate code for importing directories automatically
Copyright   : (C) 2015 Cindy Wang (CindyLinz)
License     : MIT
Maintainer  : Cindy Wang (CindyLinz) <cindylinz@gmail.com>
Stability   : beta
Portability : POSIX / Windows

This module helps to automatically generate code for importing all the haskell files from directories.

= Synopsis

For <https://www.haskell.org/cabal/ cabal> inited project, we customize @Setup.hs@ file to generate the importing code.

  * Be sure to modify the @build-type@ field in the @.cabal@ file from @Simple@ to @Custom@.

  * Then modify the @main@ function in @Setup.hs@ to generate importing code by either header file or a module file.

      Setup.hs:

      > import Distribution.Simple
      > import System.Imports (writeImportsHeader, writeImportsModule)
      >
      > main = do
      >   writeImportsHeader "imports.header" "Export" "Some.Where" "Some/Where"
      >   -- or
      >   writeImportsModule "ImportAll.hs" "ImportAll" "Some.Where" "Some/Where"
      >
      >   defaultMain

      Target.hs: (by header)

      @
      {\-\# LANGUAGE CPP \#-\}
      module Target where

      #include "imports.header"

      func = Export.funcFromSomeWhere
      @

      Target.hs: (by module)

      @
      module Target where

      import qualified ImportAll

      func = ImportAll.funcFromSomeWhere
      @
-}
module System.Imports
  ( Predictor
  , defaultPred
  , searchImportsWith
  , searchImports
  , importsContentWith
  , importsContent
  , writeMultiImportsHeaderWith
  , writeMultiImportsHeader
  , writeImportsHeaderWith
  , writeImportsHeader
  , writeMultiImportsModuleWith
  , writeMultiImportsModule
  , writeImportsModuleWith
  , writeImportsModule
  ) where

import System.Directory
import System.FilePath
import System.IO

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Except
import Control.Exception

import Data.Monoid
import Data.Either

type Predictor
  = FilePath -- ^ relative path from search root: "Bar\/Ex\/Ex2\/Foo.hs" or "Bar\/Ex\/Ex2" (you should determine the path is whether a file or a directory)
  -> IO Bool

-- | The default predictor will skip files or directories whose names are beginned with \'.\' or \'_\'.
--   And it will take only files whose extension are \".hs\" or \".lhs\"
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

-- | \"Abc\/Def\/Ghi.hs\" -> \"Abc.Def.Ghi\"
pathToModule :: FilePath -> String
pathToModule path = go $ dropExtensions path where
  go [] = []
  go (a:as)
    | isPathSeparator a = '.' : go as
    | otherwise = a : go as

patchFile :: FilePath -> String -> IO ()
patchFile path content = do
  oriContent <- flip catch ((\_ -> return Nothing) :: IOException -> IO (Maybe String)) $ do
    cont <- readFile path
    length cont `seq` return (Just cont)
  when (oriContent /= Just content) (writeFile path content)

searchImportsWith
  :: Predictor
  -> FilePath -- ^ path to the search root
  -> IO [String] -- ^ something like [\"Foo\", \"Foo.Bar\", \"Foo.Bar2\"], relative to the search root
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
  -> IO [String] -- ^ something like [\"Foo\", \"Foo.Bar\", \"Foo.Bar2\"], relative to the search root
searchImports = searchImportsWith defaultPred

importsContentWith
  :: Predictor
  -> String -- ^ import alias
  -> [(String, FilePath)] -- ^ \[(prefix, search root)\]
  -> IO String -- ^ \"import A as Alias\\n..\"
importsContentWith p alias sources = execWriterT $ do
  forM_ sources $ \(prefix', root) -> do
    let prefix = if null prefix' then "" else prefix' ++ "."
    imports <- lift $ searchImportsWith p root
    forM_ imports $ \im -> do
      tell $ "import " ++ prefix ++ im ++ " as " ++ alias ++ "\n"

importsContent
  :: String -- ^ import alias
  -> [(String, FilePath)] -- ^ \[(prefix, search root)\]
  -> IO String -- ^ \"import A as Alias\\n..\"
importsContent = importsContentWith defaultPred

writeMultiImportsHeaderWith
  :: Predictor
  -> FilePath -- ^ import header file to write
  -> String -- ^ import alias
  -> [(String, FilePath)] -- ^ \[(module name prefix, path to the search root)\]
  -> IO ()
writeMultiImportsHeaderWith p headerPath alias sources = do
  headerContent <- importsContentWith p alias sources
  patchFile headerPath headerContent

writeMultiImportsHeader
  :: FilePath -- ^ import header file to write
  -> String -- ^ import alias
  -> [(String, FilePath)] -- ^ \[(module name prefix, path to the search root)\]
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
  -> FilePath -- ^ module file to write
  -> String -- ^ module name
  -> [(String, FilePath)] -- ^ \[(module name prefix, path to the search root)\]
  -> IO ()
writeMultiImportsModuleWith p modulePath moduleName sources = do
  headerContent <- importsContentWith p "Export" sources
  patchFile modulePath $ "module " ++ moduleName ++ " (module Export) where\n" ++ headerContent

writeMultiImportsModule
  :: FilePath -- ^ module file to write
  -> String -- ^ module name
  -> [(String, FilePath)] -- ^ \[(module name prefix, path to the search root)\]
  -> IO ()
writeMultiImportsModule = writeMultiImportsModuleWith defaultPred

writeImportsModuleWith
  :: Predictor
  -> FilePath -- ^ module file to write
  -> String -- ^ module name
  -> String -- ^ module name prefix
  -> FilePath -- ^ path to the search root
  -> IO ()
writeImportsModuleWith p modulePath moduleName prefix rootPath =
  writeMultiImportsModuleWith p modulePath moduleName [(prefix, rootPath)]

writeImportsModule
  :: FilePath -- ^ module file to write
  -> String -- ^ module name
  -> String -- ^ module name prefix
  -> FilePath -- ^ path to the search root
  -> IO ()
writeImportsModule = writeImportsModuleWith defaultPred
