module Rand(rand01,randVec) where

import Prelude hiding ((<*>))
import System.Random
import Math

rand01 :: Float -> Float
rand01 f = 
    let seed = round (f* 1e8) in
    let randList =  randoms (mkStdGen seed) :: [Float] in
    head randList

randVec :: Vec3 -> Vec3
randVec (x,y,z) = 
    let xx = rand01 x in
    let yy = rand01 y in
    let zz = rand01 z in
        normalize ((xx,yy,zz) <*> 2 <-> (1,1,1))
    
