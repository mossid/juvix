module Options where

import           Foundation
import           Options.Applicative
import qualified Prelude             as P

newtype Options = Options {
  optionsCommand ∷ Command
}

data Command
  = Compile P.String P.String

options ∷ Parser Options
options = Options `fmap` commandOptions

commandOptions ∷ Parser Command
commandOptions = subparser (command "compile" (info compileOptions (progDesc "Compile a Juvix source file")))

compileOptions ∷ Parser Command
compileOptions = Compile <$> argument str (metavar "JUVIXFILE" <> help "Juvix file to compile (suggested extension: .jvx)") <*> argument str (metavar "MICHELSONFILE" <> help "Michelson file to write to (suggested extension: .tz)")
