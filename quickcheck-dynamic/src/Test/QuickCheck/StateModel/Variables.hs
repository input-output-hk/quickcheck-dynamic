{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.QuickCheck.StateModel.Variables (
  Var,
  Any (..),
  HasVariables (..),
  HasNoVariables (..),
  VarContext,
  mkVar,
  ctxAtType,
  arbitraryVar,
  shrinkVar,
  extendContext,
  isWellTyped,
  allVariables,
  unsafeCoerceVar,
  unsafeNextVarIndex,
) where

import Data.Data
import Data.Kind
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics
import GHC.TypeLits
import GHC.Word
import Test.QuickCheck as QC

-- | A symbolic variable for a value of type `a`
newtype Var a = Var Int
  deriving (Eq, Ord, Typeable, Data)

mkVar :: Int -> Var a
mkVar = Var

instance Show (Var a) where
  show (Var i) = "var" ++ show i

-- | This type class gives you a way to get all the symbolic variables that
-- appear in a value.
class HasVariables a where
  getAllVariables :: a -> Set (Any Var)

instance HasVariables a => HasVariables (Smart a) where
  getAllVariables (Smart _ a) = getAllVariables a

instance Typeable a => HasVariables (Var a) where
  getAllVariables = Set.singleton . Some

instance (HasVariables k, HasVariables v) => HasVariables (Map k v) where
  getAllVariables = getAllVariables . Map.toList

instance HasVariables a => HasVariables (Set a) where
  getAllVariables = getAllVariables . Set.toList

newtype HasNoVariables a = HasNoVariables a

deriving via a instance Show a => Show (HasNoVariables a)
deriving via a instance Eq a => Eq (HasNoVariables a)

instance HasVariables (HasNoVariables a) where
  getAllVariables _ = mempty

deriving via HasNoVariables Integer instance HasVariables Integer
deriving via HasNoVariables Int instance HasVariables Int
deriving via HasNoVariables Char instance HasVariables Char
deriving via HasNoVariables Word8 instance HasVariables Word8
deriving via HasNoVariables Word16 instance HasVariables Word16
deriving via HasNoVariables Word32 instance HasVariables Word32
deriving via HasNoVariables Word64 instance HasVariables Word64

data Any f where
  Some :: (Typeable a, Eq (f a)) => f a -> Any f

instance Eq (Any f) where
  Some (a :: f a) == Some (b :: f b) =
    case eqT @a @b of
      Just Refl -> a == b
      Nothing   -> False

instance (forall a. Ord (f a)) => Ord (Any f) where
  compare (Some (a :: f a)) (Some (a' :: f a')) =
    case eqT @a @a' of
      Just Refl -> compare a a'
      Nothing   -> compare (typeRep a) (typeRep a')

newtype VarContext = VarCtx (Set (Any Var))
  deriving (Semigroup, Monoid) via Set (Any Var)

instance Show VarContext where
  show (VarCtx vs) =
    "[" ++ intercalate ", " (map showBinding . sortBy (comparing getIdx) $ Set.toList vs) ++ "]"
    where
      getIdx (Some (Var i)) = i
      showBinding :: Any Var -> String
      -- The use of typeRep here is on purpose to avoid printing `Var` unnecessarily.
      showBinding (Some v) = show v ++ " :: " ++ show (typeRep v)

isWellTyped :: Typeable a => Var a -> VarContext -> Bool
isWellTyped v (VarCtx ctx) = Some v `Set.member` ctx

-- TODO: check the invariant that no variable index is used
-- twice at different types. This is generally not an issue
-- because lookups take types into account (so it *shouldn't*
-- cause an issue, but it might be good practise to crash
-- if the invariant is violated anyway as it is evidence that
-- something is horribly broken at the use site).
extendContext :: Typeable a => VarContext -> Var a -> VarContext
extendContext (VarCtx ctx) v = VarCtx $ Set.insert (Some v) ctx

allVariables :: HasVariables a => a -> VarContext
allVariables = VarCtx . getAllVariables

ctxAtType :: Typeable a => VarContext -> [Var a]
ctxAtType (VarCtx vs) = [v | Some (cast -> Just v) <- Set.toList vs]

arbitraryVar :: Typeable a => VarContext -> Gen (Var a)
arbitraryVar = elements . ctxAtType

shrinkVar :: Typeable a => VarContext -> Var a -> [Var a]
shrinkVar ctx v = filter (< v) $ ctxAtType ctx

unsafeCoerceVar :: Var a -> Var b
unsafeCoerceVar (Var i) = Var i

unsafeNextVarIndex :: VarContext -> Int
unsafeNextVarIndex (VarCtx vs) = 1 + maximum (0 : [i | Some (Var i) <- Set.toList vs])

-- NOTE: This trick is taken from this blog post:
-- https://blog.csongor.co.uk/report-stuck-families/
data Dummy x
type family Break (c :: Constraint) (rep :: Type -> Type) :: Constraint where
  Break _ Dummy = ((), ())
  Break _ _ = ()

instance
  {-# OVERLAPPABLE #-}
  ( Break
      (TypeError ( 'Text "Missing instance of HasVariables for non-Generic type " ':<>: 'ShowType a))
      (Rep a)
  , Generic a
  , GenericHasVariables (Rep a)
  ) =>
  HasVariables a
  where
  getAllVariables = genericGetAllVariables . from

class GenericHasVariables f where
  genericGetAllVariables :: f k -> Set (Any Var)

instance GenericHasVariables f => GenericHasVariables (M1 i c f) where
  genericGetAllVariables = genericGetAllVariables . unM1

instance HasVariables c => GenericHasVariables (K1 i c) where
  genericGetAllVariables = getAllVariables . unK1

instance GenericHasVariables U1 where
  genericGetAllVariables _ = mempty

instance (GenericHasVariables f, GenericHasVariables g) => GenericHasVariables (f :*: g) where
  genericGetAllVariables (x :*: y) = genericGetAllVariables x <> genericGetAllVariables y

instance (GenericHasVariables f, GenericHasVariables g) => GenericHasVariables (f :+: g) where
  genericGetAllVariables (L1 x) = genericGetAllVariables x
  genericGetAllVariables (R1 x) = genericGetAllVariables x
