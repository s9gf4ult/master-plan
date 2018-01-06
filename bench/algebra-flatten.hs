
import Criterion
import Criterion.Main
import MasterPlan.Algebra

alg5 :: [Algebra Int]
alg5 = Atom <$> [1..5]

alg25 :: [Algebra Int]
alg25 =
  [ Sum alg5
  , Product alg5
  , Sequence alg5
  , Sum alg5
  , Product alg5 ]

alg125 :: [Algebra Int]
alg125 =
  [ Sum alg25
  , Product alg25
  , Sequence alg25
  , Sum alg25
  , Product alg25 ]

alg625 :: [Algebra Int]
alg625 =
  [ Sum alg125
  , Product alg125
  , Sequence alg125
  , Sum alg125
  , Product alg125 ]

alg3125 :: [Algebra Int]
alg3125 =
  [ Sum alg625
  , Product alg625
  , Sequence alg625
  , Sum alg625
  , Product alg625 ]

main :: IO ()
main = defaultMain
  [ bench "flatten 5" $ nf flatten $ Sum alg5
  , bench "flatten 25" $ nf flatten $ Sum alg25
  , bench "flatten 125" $ nf flatten $ Sum alg125
  , bench "flatten 625" $ nf flatten $ Sum alg625
  , bench "flatten 3125" $ nf flatten $ Sum alg3125
  ]
