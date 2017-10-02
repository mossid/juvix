module Main where

import qualified Data.Text.IO        as T
import           Foundation
import           Options.Applicative
import           System.Exit

import qualified Juvix               as J
import           Options

main ∷ IO ()
main = do
  let opts = info (options <**> helper) (fullDesc <> progDesc "Juvix" <> header "Juvix smart contract language compiler & toolkit")
  run =<< execParser opts

run ∷ Options → IO ()
run (Options cmd) =
  case cmd of
    (Compile i o) → do
      r ← J.compileToTz i True
      case r of
        Right r → do
          T.writeFile o r
          exitSuccess
        Left er → do
          T.putStrLn $ J.pprint er
          exitFailure
