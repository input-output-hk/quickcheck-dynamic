module Spec.DynamicLogic.LinearModel where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel

data MyState = MyState {theResource :: Maybe (Var Int)}
  deriving (Show, Eq, Generic)

instance HasVariables (Action MyState a) where
  getAllVariables Init = mempty
  getAllVariables (UseLinearly v) = getAllVariables v
  getAllVariables (UseLinearlyBadly v) = getAllVariables v

deriving instance Eq (Action MyState a)
deriving instance Show (Action MyState a)

instance StateModel MyState where
  data Action MyState a where
    Init :: Action MyState Int
    UseLinearly :: Var Int -> Action MyState Int
    UseLinearlyBadly :: Var Int -> Action MyState Int

  precondition s Init = isNothing (theResource s)
  precondition s (UseLinearly v) = theResource s == Just v
  precondition s (UseLinearlyBadly v) = False && theResource s == Just v

  arbitraryAction _ (MyState Nothing) = pure $ Some Init
  arbitraryAction ctx (MyState (Just r)) =
    oneof
      [ elements
          [ Some $ UseLinearly r
          , Some $ UseLinearlyBadly r
          ]
      , Some . UseLinearly <$> arbitraryVar ctx
      , Some . UseLinearlyBadly <$> arbitraryVar ctx
      ]

  initialState = MyState Nothing

  validFailingAction _ _ = True

  nextState _ Init v = MyState (Just v)
  nextState _ UseLinearly{} v = MyState (Just v)
  nextState _ UseLinearlyBadly{} v = MyState (Just v)

instance RunModel MyState (State Int) where
  perform _ Init _ = get
  perform _ (UseLinearly v) lkp = do
    st <- get
    when (lkp v == st) $ do
      put (st + 1)
    get
  perform _ (UseLinearlyBadly v) lkp = do
    st <- get
    put 0
    get

  postcondition _ (UseLinearlyBadly v) lkp val = pure False
  postcondition _ (UseLinearly v) lkp val = pure $ lkp v + 1 == val
  postcondition _ _ _ _ = pure True

prop_State :: Actions MyState -> Property
prop_State s =
  monadic (flip evalState (0 :: Int)) $ do
    runActions s
    assert True

foo :: IO ()
foo = do
  a <- generate $ arbitrary @(Actions MyState)
  print a
  mapM_ print (shrink a)
