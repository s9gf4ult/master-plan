module MasterPlan.Algebra where


data Algebra a
  = Sum [Algebra a]
  | Product [Algebra a]
  | Sequence [Algebra a]
  | Atom a
  deriving (Eq, Ord)
