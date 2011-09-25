module HKTests where

import Prelude hiding (fail)
import Language.HK
import Language.HK.Prelude
-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

testS = extendS (Var 0) (Integer 5) emptyS
s2 = unify (Var 1) (Var 2) testS
s3 = s2 >>= unify (Var 2) (Integer 5)
s4 = s3 >>= unify (Var 1) (Var 0)

testRun = run (\x -> (x === (Integer 5)) `or2` (x === (Integer 6)))        

testWalkList = run (\x -> 
                          exist (\y -> 
                                  (exist (\z -> 
                                           (y === z) `and2` (x === quote
                                           [y,z])))))
               
testWalkList' = run (\x -> 
                       existN 2 (\[y,z] ->
                                (y === z) `and2` (x === quote [y,z])))

testWalkList2 = run (\x -> 
                      exist (\y -> 
                              (exist (\z -> 
                                       (y === z) `and2` (x === quote
                                                         [quote [y,z],z])))))
                
testWalkList2' = run (\x ->                
                        existN 2 (\[y,z] ->
                                 (y === z) `and2` (x === quote [quote [y,z],z])))

testReify = reify (quote [Var 2,quote [Var 3,Var 1],Var 2,Var 3])

testf :: Val -> Pred
testf x = succeed `or2` testf x
          
testInf = run (\x -> testf x)

testAnd' = run (\x -> existN 2 (\[y,z] -> and' [x === y, y === z, z === Integer 5]))

testOr' = run (\x -> or' [x === Integer 4, x === String "foo", x === Bool True])


testElemo xs = run $ \x -> elemo x (makeList xs)

testAppendo = run (\q -> existN 3 (\[x,y,z] -> and' [appendo x y z, q === makeList [x,y,z]]))

{--
appendL :: Val -> Val -> Val -> Pred
appendL ls s out = or' [and' [ls === List [], s === out],
                        (exist3 (\(a,d,res) ->
                                  and' [List (a:d) === ls,
                                        List (a:res) === out,
                                        appendL d s res]))]
                                        --}
