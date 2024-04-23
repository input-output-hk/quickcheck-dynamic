{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.QuickCheck.StateModel.Variables (
  Any (..),
  Var (..),
  HasVariables (..),
  HasNoVariables (..),
  SymbolicVar,
  VarContext,
  mkVar,
  ctxAtType,
  arbitraryVar,
  shrinkVar,
  extendContext,
  isWellTyped,
  allVariables,
  isEmptyCtx,
  unsafeCoerceVar,
  unsafeNextVarIndex,

  -- * Phases
  Phase (..),

  -- ** Singletons
  SingI,
  SPhase (SDynamic, SSymbolic),
  sing,
) where

import Data.Data
import Data.Kind
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Singletons
import GHC.Generics
import GHC.TypeLits
import GHC.Word
import Test.QuickCheck as QC

-- | The phases of testing
--
-- During generation of tests, we will use 'Symbolic' actions and variables. These variables
-- represent the result of performing a call and should not be inspected.
--
-- During test execution, we wil use 'Dynamic' actions and variables, where these
-- references are concretized in real values.
data Phase = Symbolic | Dynamic

data SPhase :: Phase -> Type where
  SDynamic :: SPhase 'Dynamic
  SSymbolic :: SPhase 'Symbolic

type instance Sing @Phase = SPhase

instance SingI Dynamic where
  sing = SDynamic

instance SingI Symbolic where
  sing = SSymbolic

-- | A symbolic variable for a value of type `a`
newtype SymbolicVar a = Var Int
  deriving (Eq, Ord, Typeable, Data)

-- | A variable that will be 'Symbolic' or 'Dynamic'
data Var phase a where
  SymbolicVar :: SymbolicVar a -> Var Symbolic a
  DynamicVar :: a -> Var Dynamic a

deriving instance Eq (Var Symbolic a)
deriving instance Ord (Var Symbolic a)
deriving instance Show (Var Symbolic a)

deriving instance Eq a => Eq (Var Dynamic a)
deriving instance Ord a => Ord (Var Dynamic a)
deriving instance Show a => Show (Var Dynamic a)

mkVar :: Int -> Var Symbolic a
mkVar = SymbolicVar . Var

instance Show (SymbolicVar a) where
  show (Var i) = "var" ++ show i

-- | This type class gives you a way to get all the symbolic variables that
-- appear in a value.
class HasVariables a where
  getAllVariables :: a -> Set (Any (Var Symbolic))

instance HasVariables a => HasVariables (Smart a) where
  getAllVariables (Smart _ a) = getAllVariables a

instance Typeable a => HasVariables (Var Symbolic a) where
  getAllVariables = Set.singleton . Some
instance Typeable a => HasVariables (SymbolicVar a) where
  getAllVariables = Set.singleton . Some . SymbolicVar

instance (HasVariables k, HasVariables v) => HasVariables (Map k v) where
  getAllVariables = getAllVariables . Map.toList

instance HasVariables a => HasVariables (Set a) where
  getAllVariables = getAllVariables . Set.toList

instance (forall a. HasVariables (f a)) => HasVariables (Any f) where
  getAllVariables (Some a) = getAllVariables a

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
      Nothing -> False

instance (forall a. Ord (f a)) => Ord (Any f) where
  compare (Some (a :: f a)) (Some (a' :: f a')) =
    case eqT @a @a' of
      Just Refl -> compare a a'
      Nothing -> compare (typeRep a) (typeRep a')

newtype VarContext = VarCtx (Set (Any (Var Symbolic)))
  deriving (Semigroup, Monoid) via Set (Any (Var Symbolic))

instance Show VarContext where
  show (VarCtx vs) =
    "[" ++ intercalate ", " (map showBinding . sortBy (comparing getIdx) $ Set.toList vs) ++ "]"
    where
      getIdx :: Any (Var Symbolic) -> Int
      getIdx (Some (SymbolicVar (Var i))) = i

      showBinding :: Any (Var Symbolic) -> String
      -- The use of typeRep here is on purpose to avoid printing `Var` unnecessarily.
      showBinding (Some v) = show v ++ " :: " ++ show (typeRep v)

isEmptyCtx :: VarContext -> Bool
isEmptyCtx (VarCtx ctx) = null ctx

isWellTyped :: Typeable a => Var Symbolic a -> VarContext -> Bool
isWellTyped v (VarCtx ctx) = not (null ctx) && Some v `Set.member` ctx

-- TODO: check the invariant that no variable index is used
-- twice at different types. This is generally not an issue
-- because lookups take types into account (so it *shouldn't*
-- cause an issue, but it might be good practise to crash
-- if the invariant is violated anyway as it is evidence that
-- something is horribly broken at the use site).
extendContext :: Typeable a => VarContext -> Var Symbolic a -> VarContext
extendContext (VarCtx ctx) v = VarCtx $ Set.insert (Some v) ctx

allVariables :: HasVariables a => a -> VarContext
allVariables = VarCtx . getAllVariables

ctxAtType :: Typeable a => VarContext -> [Var Symbolic a]
ctxAtType (VarCtx vs) = [v | Some (cast -> Just v) <- Set.toList vs]

arbitraryVar :: Typeable a => VarContext -> Gen (Var Symbolic a)
arbitraryVar = elements . ctxAtType

shrinkVar :: Typeable a => VarContext -> Var Symbolic a -> [Var Symbolic a]
shrinkVar ctx v = filter (< v) $ ctxAtType ctx

unsafeCoerceVar :: Var Symbolic a -> Var Symbolic b
unsafeCoerceVar (SymbolicVar (Var i)) = SymbolicVar (Var i)

unsafeNextVarIndex :: VarContext -> Int
unsafeNextVarIndex (VarCtx vs) = 1 + maximum (0 : [i | Some (SymbolicVar (Var i)) <- Set.toList vs])

-- NOTE: This trick is taken from this blog post:
-- https://blog.csongor.co.uk/report-stuck-families/
data Dummy x
type family Break (c :: Constraint) (rep :: Type -> Type) :: Constraint where
  Break _ Dummy = ((), ())
  Break _ _ = ()

instance
  {-# OVERLAPPABLE #-}
  ( Break
      (TypeError ('Text "Missing instance of HasVariables for non-Generic type " ':<>: 'ShowType a))
      (Rep a)
  , Generic a
  , GenericHasVariables (Rep a)
  )
  => HasVariables a
  where
  getAllVariables = genericGetAllVariables . from

class GenericHasVariables f where
  genericGetAllVariables :: f k -> Set (Any (Var Symbolic))

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
