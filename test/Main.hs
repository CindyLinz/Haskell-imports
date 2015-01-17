module Main where

import System.Imports

import Control.Monad
import System.Exit (exitFailure)
import System.FilePath

main :: IO ()
main = do
  header <- importsContent "I" [("Material", "test" </> "Material")]
  unless
    ( header `elem`
      [ "import Material.B.C.D as I\nimport Material.A as I\n"
      , "import Material.A as I\nimport Material.B.C.D as I\n"
      ]
    ) exitFailure
  return ()
