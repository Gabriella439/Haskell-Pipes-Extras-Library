module Main (main) where

import Data.Functor.Identity (Identity)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Pipes (Pipe, (>->), each)
import Pipes.Prelude (toList)
import Pipes.Extras (scan1i)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Scans" [
         testCase "Scan1i works for EMA calculation" testScan1i
         ]
    ]

testScan1i :: Assertion
testScan1i = transduce (scan1i (\l i -> l * α + i * (1 - α))) [2, 0, 0, 1.5] @?= ([2, 1, 0.5, 1] :: [Double])
  where α = 0.5

transduce :: Pipe a b Identity () -> [a] -> [b]
transduce pipe xs = toList (each xs >-> pipe)
