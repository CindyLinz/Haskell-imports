# Haskell-imports

This module helps to automatically generate code for importing all the haskell files from directories.

# Synopsis

For [cabal](https://www.haskell.org/cabal/) inited project, we customize `Setup.hs` file to generate the importing code.

  * Be sure to modify the `build-type` field in the `.cabal` file from `Simple` to `Custom`.

  * Then modify the `main` function in `Setup.hs` to generate importing code by either header file or a module file.

      Setup.hs:

      ```haskell
      import Distribution.Simple
      import System.Imports (writeImportsHeader, writeImportsModule)

      main = do
        writeImportsHeader "imports.header" "Export" "Some.Where" "Some/Where"
        -- or
        writeImportsModule "ImportAll.hs" "ImportAll" "Some.Where" "Some/Where"

        defaultMain
      ```

      Target.hs: (by header)

      ```haskell
      {-# LANGUAGE CPP #-}
      module Target where

      #include "imports.header"

      func = Export.funcFromSomeWhere
      ```

      Target.hs: (by module)

      ```haskell
      module Target where

      import qualified ImportAll

      func = ImportAll.funcFromSomeWhere
      ```

# Hackage Link (for Haddock)

[imports on hackage](http://hackage.haskell.org/package/imports)

# License

Copyright 2015, Cindy Wang (CindyLinz) Licensed under the MIT license.
