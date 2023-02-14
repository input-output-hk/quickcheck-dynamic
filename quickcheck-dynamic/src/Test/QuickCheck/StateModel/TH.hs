{-# LANGUAGE CPP #-}

module Test.QuickCheck.StateModel.TH (makeActionInstances) where

import Control.Monad
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax hiding (Type)

makeHasVarsInstance :: TH.Type -> [Con] -> Q InstanceDec
makeHasVarsInstance typ cs = do
  Just hasVarsName <- lookupTypeName "HasVariables"
  Just getAllVarsName <- lookupValueName "getAllVariables"
  let mkClause (NormalC cn args) = mkClauseWith cn (length args)
      mkClause (RecC cn args) = mkClauseWith cn (length args)
      mkClause (InfixC _ cn _) = mkClauseWith cn 2
      mkClause (ForallC _ _ con) = mkClause con
      mkClause (GadtC (cn : _) args _) = mkClauseWith cn (length args)
      mkClause (RecGadtC (cn : _) args _) = mkClauseWith cn (length args)
      mkClause _ = error "The impossible happened"
      mkClauseWith cn n = do
        names <- replicateM n $ qNewName "a"
        buildBody cn names
  cls <- mapM mkClause cs
  pure $ InstanceD Nothing [] (AppT (ConT hasVarsName) typ) [FunD getAllVarsName cls]

buildBody :: Name -> [Name] -> Q Clause
#if __GLASGOW_HASKELL__ >= 902
buildBody cn names = do
  body <- buildBody' names
  pure $ Clause [ConP cn [] (map VarP names)] body []
#else
buildBody cn names = do
  body <- buildBody' names
  pure $ Clause [ConP cn (map VarP names)] body []
#endif

buildBody' :: [Name] -> Q Body
buildBody' [] = do
  Just memptyName <- lookupValueName "mempty"
  pure $ NormalB $ VarE memptyName
buildBody' as = do
  Just mappendName <- lookupValueName "mappend"
  Just getAllVarsName <- lookupValueName "getAllVariables"
  pure $
    NormalB $
      foldr1
        (AppE . AppE (VarE mappendName))
        (map (AppE (VarE getAllVarsName) . VarE) as)

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
