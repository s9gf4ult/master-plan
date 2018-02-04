module MasterPlan.Algebra where

import Control.DeepSeq
import GHC.Generics (Generic)

data Algebra a
  = Sum [Algebra a]
  | Product [Algebra a]
  | Sequence [Algebra a]
  | Atom a
  deriving (Eq, Show, Ord, Generic, Functor, Foldable)

instance (NFData a) => NFData (Algebra a)

flatten :: Algebra a -> Algebra a
flatten = \case
  Atom a      -> Atom a
  Sum as      -> Sum      $ fmap flatten $ as >>= sums
  Product as  -> Product  $ fmap flatten $ as >>= prods
  Sequence as -> Sequence $ fmap flatten $ as >>= seqs
  where
    sums, prods, seqs :: Algebra a -> [Algebra a]
    sums = \case
      Sum as -> as >>= sums
      a      -> [a]
    prods = \case
      Product as -> as >>= prods
      a          -> [a]
    seqs = \case
      Sequence as -> as >>= seqs
      a           -> [a]
