module Juvix.Transpiler.Encoding where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Text                as T
import           Foundation
import qualified Prelude                  as P

import qualified Juvix.Michelson.Script   as M
import           Juvix.Types
import           Juvix.Utility

pack ∷ M.Type → CompilerM Expr
pack M.UnitT = return (Lit LUnit)
pack ty      = throwError (NotYetImplemented ("pack: " `T.append` pprint ty))

-- Start with value to unpack at top of stack. len (filter Just binds) will be dropped at the end.

unpack ∷ M.Type → [Maybe T.Text] → CompilerM M.ExprUT
unpack ty [] | ty == M.UnitT = do
  modify (drop 1)
  return M.DropUT
unpack ty [Nothing]   | ty `elem` unitaryTypes = do
  modify (drop 1)
  return M.DropUT
unpack ty [Just bind] | ty `elem` unitaryTypes = do
  modify ((:) (BoundVariable bind) . drop 1)
  return M.NopUT
unpack ty@(M.PairT _ _) binds =
  case binds of
    [Just fst, Just snd] → do
      modify ((<>) [BoundVariable fst, BoundVariable snd] . drop 1)
      return (M.SeqUT (M.SeqUT (M.SeqUT M.DupUT M.CdrUT) M.SwapUT) M.CarUT)
    [Just fst, Nothing] → do
      modify ((:) (BoundVariable fst) . drop 1)
      return M.CarUT
    [Nothing, Just snd] → do
      modify ((:) (BoundVariable snd) . drop 1)
      return M.CdrUT
    _ → throwError (NotYetImplemented (T.concat ["unpack: ", pprint ty, " ~ ", T.intercalate ", " (fmap pprint binds)]))
unpack ty binds = throwError (NotYetImplemented (T.concat ["unpack: ", pprint ty, " ~ ", T.intercalate ", " (fmap pprint binds)]))

unpackDrop ∷ [Maybe T.Text] → CompilerM M.ExprUT
unpackDrop binds = do
  let count      = P.length (P.filter isJust binds)
      foldDrop 0 = M.NopUT
      foldDrop 1 = M.SeqUT M.SwapUT M.DropUT
      foldDrop n = M.SeqUT (foldDrop (n - 1)) (foldDrop 1)
  modify (\(x:xs) → x : P.drop count xs)
  return (foldDrop count)

unitaryTypes ∷ [M.Type]
unitaryTypes = [M.UnitT, M.IntT, M.TezT, M.KeyT]
