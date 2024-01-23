import Test.Tasty
import Test.Tasty.HUnit

import Control.Parallel

import qualified Par
import qualified Seq

parnofib :: Int -> Int
parnofib 0 = 1
parnofib 1 = 1
parnofib n =
    let n1 = parnofib (n - 1)
        n2 = parnofib (n - 2)
    in n1 `par` n2 `pseq` n1 + n2 + 1

main :: IO ()
main = defaultMain $ testGroup "parallel"
    [ testGroup "Control.Parallel"
        [ testCase "parnofib" $ parnofib 30 @?= 2692537
        ]
    , Par.tests
    , Seq.tests
    ]
