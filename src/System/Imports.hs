{- |
This module helps to generate code for importing all the haskell files from directories.

= Synopsis

There're primarily two ways to trigger the code generating process:

  * For simple haskell project, we use <https://www.haskell.org/haskellwiki/Template_Haskell TemplateHaskell> to generate the importing code
    while compiling the file.

    There're 2 approaches:

      * Writes the import header to a header file. And then we @#include@ the header (by @CPP@ extension) in.

        @
        {&#45;&#35; LANUGAGE TemplateHaskell, CPP &#35;&#45;}
        module Foo where

        import System.Import (writeImportsHeader)

        $(writeImportsHeader "imports.header" &#34;I&#34; &#34;Some.Where&#34; &#34;Some/Where&#34;)
        #include "imports.header"

        func = I.someFuncInSomeWhere
        @

      * Or, create the whole module file.

        @
        {&#45;&#35; LANUGAGE TemplateHaskell &#35;&#45;}
        module Foo where

        import System.Import (writeImportsModule)
        $(writeImportsModule &#34;MyImport.hs&#34; &#34;MyImport&#34; &#34;Some.Where&#34; &#34;Some/Where&#34;)

        import qualified MyImport

        func = MyImport.someFuncInSomeWhere
        @

  * For <https://www.haskell.org/cabal/ cabal> inited project, we customize @Setup.hs@ file to generate the importing code.

    * Be sure to modify the @build-type@ field in the @.cabal@ file from @Simple@ to @Custom@.

    * Then modify the @main@ function in @Setup.hs@ to generate importing code by either header file or a whole module file
      explained above.
-}
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

pathToModule :: FilePath -> String
pathToModule path = go $ dropExtensions path where
  go [] = []
  go (a:as)
    | isPathSeparator a = '.' : go as
    | otherwise = a : go as

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
  -> IO String
importsContentWith p alias sources = execWriterT $ do
  forM_ sources $ \(prefix', root) -> do
    let prefix = if null prefix' then "" else prefix' ++ "."
    imports <- lift $ searchImportsWith p root
    forM_ imports $ \im -> do
      tell $ "import qualified " ++ prefix ++ im ++ " as " ++ alias ++ "\n"

importsContent
  :: String -- ^ import alias
  -> [(String, FilePath)] -- ^ \[(prefix, search root)\]
  -> IO String
importsContent = importsContentWith defaultPred

writeMultiImportsHeaderWith
  :: Predictor
  -> FilePath -- ^ import header file to write
  -> String -- ^ import alias
  -> [(String, FilePath)] -- ^ \[(module name prefix, path to the search root)\]
  -> IO ()
writeMultiImportsHeaderWith p headerPath alias sources = do
  headerContent <- importsContentWith p alias sources
  writeFile headerPath headerContent

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
  writeFile modulePath $ "module " ++ moduleName ++ " (module Export) where\n" ++ headerContent

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
