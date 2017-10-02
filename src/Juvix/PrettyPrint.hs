module Juvix.PrettyPrint where

import qualified ConLike             as GHC
import qualified CoreSyn             as GHC
import qualified DataCon             as GHC
import qualified DynFlags            as GHC
import qualified FastString          as GHC
import qualified GHC
import qualified Literal             as GHC
import qualified Name                as GHC hiding (varName)
import qualified Outputable          as GHC
import qualified TyCon               as GHC
import qualified TyCoRep             as GHC
import qualified UniqFM              as GHC
import qualified Unique              as GHC
import qualified Var                 as GHC

import qualified Data.Map            as Map
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Foundation
import           Juvix.Emit          (emitUT)
import qualified Juvix.Script        as J (ExprUT, Type (..))
import qualified Juvix.Types         as J (CompileError (..), CompileLog (..),
                                           Expr (..), StackObject (..))
import qualified Prelude             as P
import qualified System.Console.ANSI as ANSI

import qualified GHC.Paths           as Paths
import           System.IO.Unsafe

{-  This should be changed, although the fault lies equally with GHC's arcane "Outputable" API.   -}

{-# NOINLINE dynFlags #-}
dynFlags ∷ GHC.DynFlags
dynFlags = unsafePerformIO (GHC.runGhc (Just Paths.libdir) GHC.getDynFlags)

withSGR ∷ [ANSI.SGR] → T.Text → T.Text
withSGR sgr str = T.concat [T.pack (ANSI.setSGRCode sgr), str, T.pack (ANSI.setSGRCode [ANSI.Reset])]

withColor ∷ ANSI.Color → T.Text → T.Text
withColor color = withSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull color]

ppOutputable ∷ (GHC.Outputable a) ⇒ a → T.Text
ppOutputable = T.pack . GHC.showSDoc dynFlags . GHC.ppr

ppCompileLog ∷ J.CompileLog → T.Text
ppCompileLog (J.FrontendToCore core)      = T.concat [withColor ANSI.Magenta "Frontend ⇒ Core :\n\t=> ", pprint core]
ppCompileLog (J.CoreToExpr core expr)     = T.concat [withColor ANSI.Magenta "Core ⇒ Expr : ", pprint core, withColor ANSI.Magenta "\n\t⇒ ", pprint expr]
ppCompileLog (J.SimplifiedExpr a b)       = T.concat [withColor ANSI.Magenta "Expr ⇒ Expr : ", pprint a, withColor ANSI.Magenta "\n\t⇒ ", pprint b]
ppCompileLog (J.ExprToMichelson a b x y)  = T.concat [withColor ANSI.Magenta "Expr ⇒ Michelson : ", pprint a, withColor ANSI.Magenta "\n\t⇒ ", pprint b, "\n\t~ [", T.intercalate ", " (fmap pprint x), "] ⇒ [", T.intercalate "," (fmap pprint y), "]"]
ppCompileLog (J.Optimized a b)            = T.concat [withColor ANSI.Magenta "Michelson ⇒ Michelson : ", pprint a, withColor ANSI.Magenta "\n\t⇒ ", pprint b]

ppStackObject ∷ J.StackObject → T.Text
ppStackObject J.FuncResult        = withColor ANSI.Cyan "R"
ppStackObject (J.Const c)         = T.concat [withColor ANSI.Yellow "Const ", pprint c]
ppStackObject (J.BoundVariable v) = withColor ANSI.Green v

ppCompileError ∷ J.CompileError → T.Text
ppCompileError J.InvalidType              = withColor ANSI.Red "Invalid type"
ppCompileError J.InvariantError           = withColor ANSI.Red "Invariant error"
ppCompileError J.MainFunctionNotFound     = withColor ANSI.Red "Main function not found"
ppCompileError (J.VariableNotInScope v s) = T.concat [withColor ANSI.Red "Variable not in scope: ", withColor ANSI.Green v, "; stack [", T.intercalate ", " (fmap pprint s), "]"]
ppCompileError (J.NotYetImplemented e)    = T.concat [withColor ANSI.Red "Not yet implemented: ", e]

ppJExpr ∷ J.Expr → T.Text
ppJExpr (J.BuiltIn e) = T.concat [withColor ANSI.Cyan "BuiltIn {", pprint e, withColor ANSI.Cyan "}"]
ppJExpr (J.Let x y z) = T.concat ["let ", withColor ANSI.Green x, " = ", pprint y, " in ", pprint z]
ppJExpr (J.Var v)     = withColor ANSI.Green v
ppJExpr (J.Lit e)     = T.concat [withColor ANSI.Cyan "Literal {", pprint e, withColor ANSI.Cyan "}"]
ppJExpr (J.App x y)   = T.concat ["(", pprint x, ") (", pprint y, ")"]
ppJExpr (J.Lam v e)   = T.concat [withColor ANSI.Yellow "\\", withColor ANSI.Green v, withColor ANSI.Yellow " → ", pprint e]
ppJExpr (J.If x y z)  = T.concat [withColor ANSI.Cyan "If", " {", pprint x, "} {", pprint y, "} {", pprint z, "}"]
ppJExpr (J.LitCast x y) = T.concat [withColor ANSI.Cyan "LitCast", " @", pprint x, " @", pprint y, ""]

ppTyThing ∷ GHC.TyThing → T.Text
ppTyThing (GHC.AnId v)     = T.concat ["VarT {", pprint v, "}"]
ppTyThing (GHC.AConLike c) = T.concat ["ConLikeT {", pprint c, "}"]
ppTyThing (GHC.ATyCon c)   = T.concat ["TyConT {", pprint c, "}"]
ppTyThing _                = T.concat ["TyThing"]

ppConLike ∷ GHC.ConLike → T.Text
ppConLike _ = T.concat ["ConLike"]

ppMod ∷ GHC.CoreModule → T.Text
ppMod (GHC.CoreModule _ nameEnv bs _) = T.unlines [
  "TypeEnv: ",
  T.intercalate "\n" $ fmap (\(x, y) → T.concat [pprint x, " ⇒ ", pprint y]) $ GHC.ufmToList nameEnv,
  "Bindings: ",
  T.intercalate "\n" $ fmap pprint bs
  ]

ppUnique ∷ GHC.Unique → T.Text
ppUnique _ = "Unique"

ppExpr ∷ GHC.CoreExpr → T.Text
ppExpr = ppOutputable
--ppExpr = ppExpr'

ppExpr' ∷ GHC.CoreExpr → T.Text
ppExpr' (GHC.Var v)          = T.concat ["VarE (", pprint v, ")"]
ppExpr' (GHC.Lit l)          = T.concat ["LitE (", pprint l, ")"]
ppExpr' (GHC.App x y)        = T.concat ["AppE (", ppExpr x, ") (", ppExpr y, ")"]
ppExpr' (GHC.Lam x y)        = T.concat ["LamE (", pprint x, ") (", ppExpr y, ")"]
ppExpr' (GHC.Let b e)        = T.concat ["LetE (", pprint b, ") (", ppExpr e, ")"]
ppExpr' (GHC.Case e v t xs)  = T.concat ["CaseE (", ppExpr e, ") (", pprint v, ") (", pprint t, ") [", T.intercalate ", " $ fmap pprint xs, "]"]
ppExpr' (GHC.Tick _ e)       = T.concat ["TickE (", ppExpr e, ")"]
ppExpr' (GHC.Type t)         = T.concat ["TypeE (", pprint t, ")"]
ppExpr' (GHC.Coercion _)     = T.concat ["CoercionE ()"]
ppExpr' _                    = T.concat ["Expr..."]

ppAlt ∷ GHC.Alt GHC.CoreBndr → T.Text
ppAlt (x, v, e) = T.concat ["{", pprint x, " - ", T.intercalate ", " $ fmap pprint v, " - ", pprint e, "}"]

ppAltCon ∷ GHC.AltCon → T.Text
ppAltCon (GHC.DataAlt d) = T.concat ["DataAltCon (", pprint d, ")"]
ppAltCon (GHC.LitAlt l)  = T.concat ["LitAltCon (", pprint l, ")"]
ppAltCon GHC.DEFAULT     = T.concat ["AltConDefault"]

ppDataCon ∷ GHC.DataCon → T.Text
ppDataCon = pprint . GHC.dataConName

ppLit ∷ GHC.Literal → T.Text
ppLit (GHC.MachChar c)        = T.concat ["CharL (", T.pack [c], ")"]
ppLit (GHC.MachStr b)         = T.concat ["StrL (", T.decodeUtf8 b, ")"]
ppLit GHC.MachNullAddr        = T.concat ["NullL"]
ppLit (GHC.MachInt i)         = T.concat ["IntL (", T.pack $ P.show i, ")"]
ppLit (GHC.MachInt64 i)       = T.concat ["Int64L (", T.pack $ P.show i, ")"]
ppLit (GHC.MachWord i)        = T.concat ["WordL (", T.pack $ P.show i, ")"]
ppLit (GHC.MachWord64 i)      = T.concat ["Word64L (", T.pack $ P.show i, ")"]
ppLit (GHC.MachFloat r)       = T.concat ["FloatL (", T.pack $ P.show r, ")"]
ppLit (GHC.MachDouble r)      = T.concat ["DoubleL (", T.pack $ P.show r, ")"]
ppLit (GHC.MachLabel f mi _)  = T.concat ["LabelL (", T.decodeUtf8 $ GHC.fs_bs f, ") (", T.pack $ P.show mi, ") ()"]
ppLit (GHC.LitInteger i t)    = T.concat ["IntegerL (", T.pack $ P.show i, ") @ (", pprint t, ")"]

ppVar ∷ GHC.Var → T.Text
ppVar = nameToText . GHC.varName

typeName ∷ GHC.Type → GHC.Name
typeName (GHC.TyVarTy v)    = GHC.varName v
typeName (GHC.AppTy _ y)    = typeName y
typeName (GHC.TyConApp c _) = GHC.tyConName c
typeName (GHC.CastTy x _)   = typeName x
typeName (GHC.ForAllTy _ t) = typeName t

ppType ∷ GHC.Type → T.Text
ppType (GHC.TyVarTy v) = pprint v
ppType (GHC.AppTy x y) = T.concat ["AppT (", ppType x, ") (", ppType y, ")"]
ppType (GHC.TyConApp c t) = T.concat ["ConT (", pprint c, ") [", T.intercalate "," $ fmap pprint t, "]"]
ppType _               = T.concat ["AnotherType"] -- TODO

ppTycon ∷ GHC.TyCon → T.Text
ppTycon c =
  let n = nameToText $ GHC.tyConName c
  in if
    | GHC.isFunTyCon c → T.concat ["FunT (", n, ")"]
    | GHC.isAlgTyCon c → T.concat ["AlgT (", n, ")"]
    | GHC.isTypeSynonymTyCon c → T.concat ["SynT (", n, ")"]
    | GHC.isFamilyTyCon c → T.concat ["FamT (", n, ")"]
    | GHC.isPrimTyCon c → T.concat ["PrimT (", n, ")"]
    | GHC.isPromotedDataCon c → T.concat ["PromT (", n, ")"]
    | GHC.isTcTyCon c → T.concat ["TcTyT (", n, ")"]

ppBind ∷ GHC.CoreBind → T.Text
ppBind (GHC.NonRec b e) = T.concat ["NonRecB (", pprint b, ") (", pprint e, ")"]
ppBind (GHC.Rec xs)     = T.concat ["RecB [", T.concat $ fmap (\(x, y) → T.concat ["(", pprint x, ") (", pprint y, ")"]) xs, "]"]

nameToText ∷ GHC.Name → T.Text
nameToText name =
  let occName = GHC.nameOccName name in
  T.concat [T.decodeUtf8 $ GHC.fs_bs $ GHC.occNameFS occName, "_", T.pack $ P.show $ GHC.getUnique occName]

nameToTextSimple ∷ GHC.Name → T.Text
nameToTextSimple = T.decodeUtf8 . GHC.fs_bs . GHC.occNameFS . GHC.nameOccName

class PrettyPrint a where
  pprint ∷ a → T.Text

instance PrettyPrint GHC.CoreExpr where pprint = ppExpr
instance PrettyPrint GHC.Var where pprint = ppVar
instance PrettyPrint GHC.Type where pprint = ppType
instance PrettyPrint GHC.TyCon where pprint = ppTycon
instance PrettyPrint GHC.Literal where pprint = ppLit
instance PrettyPrint GHC.CoreBind where pprint = ppBind
instance PrettyPrint GHC.CoreModule where pprint = ppMod
instance PrettyPrint (GHC.Alt GHC.CoreBndr) where pprint = ppAlt
instance PrettyPrint GHC.AltCon where pprint = ppAltCon
instance PrettyPrint GHC.TyThing where pprint = ppTyThing
instance PrettyPrint GHC.ConLike where pprint = ppConLike
instance PrettyPrint GHC.Unique where pprint = ppUnique
instance PrettyPrint GHC.Name where pprint = nameToText
instance PrettyPrint GHC.DataCon where pprint = ppDataCon
instance PrettyPrint J.Expr where pprint = ppJExpr
instance PrettyPrint J.CompileLog where pprint = ppCompileLog
instance PrettyPrint J.ExprUT where pprint = emitUT
instance PrettyPrint J.CompileError where pprint = ppCompileError
instance PrettyPrint J.StackObject where pprint = ppStackObject
instance PrettyPrint J.Type where pprint = T.pack . P.show

instance PrettyPrint T.Text where pprint = id

instance (PrettyPrint k, PrettyPrint v) ⇒ PrettyPrint (Map.Map k v) where
  pprint = (\x → T.concat ["{ ", x, " }"]) . T.intercalate ", " . fmap (\(x, y) → T.concat [pprint x, " ⇒ ", pprint y]) . Map.toList
