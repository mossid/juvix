module Main where

import           Data.List                    (nub, (++))
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Foundation                   hiding (show)
import           Prelude                      (mapM, unlines)
import qualified System.Directory             as D
import qualified Test.Tasty                   as T
import qualified Test.Tasty.HUnit             as T

import qualified Juvix                        as J
import qualified Juvix.Michelson.Optimization as J
import qualified Juvix.Michelson.Script       as J
import           Juvix.Utility

{-  I'm more inclined to focus time on improving the degree to which desired correctness properties are enforced by the GHC typechecker than writing large numbers of test cases, but a few are still nice for sanity.
    At the moment, the test suite checks that a few simple Haskell programs compile to correct (expected) Michelson. To add a test case, just add "{testname}.hs" and "{testname}.tz.expected" files in a subdirectory of "code".
    Better future versions might:
    - Enumerate over equivalent expressions (e.g. f ⇒ \x → f x) and ensure equivalence of output
    - Run the output Michelson programs (easier once the Haskell interpreter is complete) using e.g. QuickCheck   -}

main ∷ IO ()
main = do

  pwd ← D.getCurrentDirectory
  let base = pwd ++ "/test/code"
  sub ← D.listDirectory base
  all ← mapM (\d → (,) d `fmap` D.listDirectory (base ++ "/" ++ d)) sub
  let dropExtension = takeWhile ('.' /=)
  loaded ← flip mapM all $ \(d, f) → do
              let unique = nub $ fmap dropExtension f
              tests ← flip mapM unique $ \name → do
                let hsf = base ++ "/" ++ d ++ "/" ++ name ++ ".hs"
                    mcf = base ++ "/" ++ d ++ "/" ++ name ++ ".tz.expected"
                return $ T.testCase name $ do
                      mcl ← T.readFile mcf
                      cmp ← J.compileToTz hsf False
                      (case cmp of Right x → x == mcl; _ → False) T.@? unlines [
                        "Compilation output and expected output do not match",
                        "Compilation output:",
                        case cmp of Right v → T.unpack v; Left e → T.unpack (pprint e),
                        "Expected output:",
                        T.unpack mcl
                        ]
              return $ T.testGroup d tests

  let fullCompilerTests = T.testGroup "Full Compiler" loaded

  T.defaultMain (tests fullCompilerTests)

tests ∷ T.TestTree → T.TestTree
tests fullCompilerTests = T.testGroup "Tests" [optimizationTests, fullCompilerTests]

optimizationTests ∷ T.TestTree
optimizationTests = T.testGroup "Optimization" [

  T.testCase "NOP removed" (J.optimizeNoLogs (J.Seq J.Nop J.Nop) == (J.Nop ∷ J.Expr (J.Stack ()) (J.Stack ())) T.@? "NOP was not removed")

  ]

-- Rewrite as actual Haskell, forget the file reading.
-- data TestCase = ...
-- Just inline the Haskell code.
-- OR: Compile both Haskell standard and Michelson, ensure same properties!
-- ^ good, but can we save this concept?
