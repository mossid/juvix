module Main where

import qualified Data.Text                    as T
import           Data.Typeable
import           Foundation                   hiding (show)
import           Prelude                      (mapM_)
import qualified System.IO                    as IO
import qualified System.IO.Temp               as Temp
import qualified Test.Tasty                   as T
import qualified Test.Tasty.HUnit             as T

import qualified Juvix                        as J
import qualified Juvix.Michelson.Emit         as J
import qualified Juvix.Michelson.Interpreter  as J
import qualified Juvix.Michelson.Optimization as J
import qualified Juvix.Michelson.Script       as J

import           Code
import           Types

{-  I'm more inclined to focus time on improving the degree to which desired correctness properties are enforced by the GHC typechecker than writing large numbers of test cases, but a few are still nice for sanity.
    Better future versions might:
    - Enumerate over equivalent expressions (e.g. f ⇒ \x → f x) and ensure equivalence of output
    - Run the output Michelson programs (easier once the Haskell interpreter is complete) using e.g. QuickCheck   -}

main ∷ IO ()
main = T.defaultMain (T.testGroup "Tests" [optimizationTests, transpilationTests])

transpilationTests ∷ T.TestTree
transpilationTests = T.testGroup "Transpilation" (fmap transpilationTestCase transpilationTestCases)

optimizationTests ∷ T.TestTree
optimizationTests = T.testGroup "Optimization" [

  T.testCase "NOP removed" (J.optimizeNoLogs (J.Seq J.Nop J.Nop) == (J.Nop ∷ J.Expr (J.Stack ()) (J.Stack ())) T.@? "NOP was not removed")

  ]

wrappedInterpret ∷ J.SomeExpr → J.SomeType → J.Tez → J.Timestamp → J.SomeType → T.Assertion
wrappedInterpret (J.SomeExpr (expr ∷ J.Expr (J.Stack a) (J.Stack b))) (J.SomeType (arg ∷ argType)) amount timestamp (J.SomeType (ret ∷ retType)) =
  case (eqT ∷ Maybe (a :~: (argType, ())), eqT ∷ Maybe (b :~: (retType, ()))) of
    (Nothing, _) → T.assertFailure ("Failed to unify argument type: expected " <> T.unpack (J.pprintTy arg))
    (_, Nothing) → T.assertFailure ("Failed to unify return type: expected " <> T.unpack (J.pprintTy ret))
    (Just Refl, Just Refl) ->
      let origination = J.OriginationNonce () 0
          context = J.Storage
          result ∷ Either J.InterpretError (retType, Int, J.Context, J.OriginationNonce)
          result = J.interpret origination maxBound undefined undefined amount context (J.LambdaW expr) arg
      in case result of
        Right (res, _, _, _) → T.assertBool ("Expected output did not match: expected " <> T.unpack (J.pprintEx ret) <> " but instead got " <> T.unpack (J.pprintEx res)) (res == ret)
        Left err             → T.assertFailure ("Interpretation failed with error: " <> T.unpack (J.pprint err))

transpilationTestCase ∷ TranspilationTestCase → T.TestTree
transpilationTestCase (TranspilationTestCase name haskell inputs) =
  T.testCaseSteps name $ \step → do
    path ← Temp.emptySystemTempFile "juvix.hs"
    step "Transpiling to Michelson..."
    IO.writeFile path haskell
    compileResult ← J.compileToTyped path
    case compileResult of
      Left err → T.assertFailure ("Compilation failed with error: " <> T.unpack (J.pprint err))
      Right (someExpr, paramTy, retTy, storageTy) → do
        step ("Transpilation OK; param type " <> T.unpack (J.pprint paramTy) <> ", return type " <> T.unpack (J.pprint retTy) <> ", storage type " <> T.unpack (J.pprint storageTy))
        step ("Result: " <> (T.unpack (case someExpr of J.SomeExpr e → J.emit e)))
        {-
        flip mapM_ inputs $ \(start@(J.SomeType s), amount, timestamp, end) → do
          step ("Testing input: " <> T.unpack (J.pprintEx s))
          wrappedInterpret someExpr start amount timestamp end
        -}
