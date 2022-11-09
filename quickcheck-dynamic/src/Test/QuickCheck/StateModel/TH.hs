module Test.QuickCheck.StateModel.TH (makeActionInstances) where

import Control.Monad
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax hiding (Type)

makeHasVarsInstance :: TH.Type -> [Con] -> Q InstanceDec
makeHasVarsInstance typ cs = do
  Just hasVarsName <- lookupTypeName "HasVariables"
  Just getAllVarsName <- lookupValueName "getAllVariables"
  Just memptyName <- lookupValueName "mempty"
  Just mappendName <- lookupValueName "mappend"
  let mkClause (NormalC cn args) = mkClauseWith cn (length args)
      mkClause (RecC cn args) = mkClauseWith cn (length args)
      mkClause (InfixC _ cn _) = mkClauseWith cn 2
      mkClause (ForallC _ _ con) = mkClause con
      mkClause (GadtC (cn : _) args _) = mkClauseWith cn (length args)
      mkClause (RecGadtC (cn : _) args _) = mkClauseWith cn (length args)
      mkClause _ = error "The impossible happened"
      mkClauseWith cn n = do
        names <- sequence $ replicate n $ qNewName "a"
        -- TODO: when we migrate to ghc 9.2 we need to put in an extra empty list here
        pure $ Clause [ConP cn (map VarP names)] (buildBody names) []
      buildBody [] = NormalB $ VarE memptyName
      buildBody as =
        NormalB $
          foldr1
            (\e e' -> AppE (AppE (VarE mappendName) e) e')
            (map (AppE (VarE getAllVarsName) . VarE) as)
  cls <- mapM mkClause cs
  pure $ InstanceD Nothing [] (AppT (ConT hasVarsName) typ) [FunD getAllVarsName cls]

makeActionInstances :: Name -> Q [Dec]
makeActionInstances stateTypeName = do
  Just actionTypeName <- lookupTypeName "Action"
  [DataInstD _ _ _ _ cs _] <- reifyInstances actionTypeName [ConT stateTypeName]
  Just eqName <- lookupTypeName "Eq"
  newVarName <- qNewName "a"
  stateTypeKind <- reifyType stateTypeName
  let numStateTypeParams = arity stateTypeKind
        where
          arity (AppT (AppT ArrowT _) k) = 1 + arity k
          arity _ = 0
  stateTypeArgs <- replicateM numStateTypeParams $ qNewName "a"
  let typ =
        AppT
          ( AppT
              (ConT actionTypeName)
              (foldl AppT (ConT stateTypeName) (VarT <$> stateTypeArgs))
          )
          (VarT newVarName)
  varsInstance <- makeHasVarsInstance typ cs
  return $
    [ varsInstance
    , StandaloneDerivD Nothing [] (AppT (ConT eqName) typ)
    ]
