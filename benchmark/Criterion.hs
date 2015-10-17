{-# OPTIONS_GHC -fdefer-typed-holes -fno-warn-orphans #-}



module Main (main) where



import Numeric.Algebra.Elementary.AST
import Numeric.Algebra.Elementary.Rewrite
import Criterion.Main


main :: IO ()
main = defaultMain [ bgroup "rewrite" [ bench "?"   $ whnf simplify undefined
                                      , bench "??"  $ whnf simplify undefined
                                      , bench "???" $ whnf simplify undefined
                                      ]
                   ]
