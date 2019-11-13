module Math( Vec3, (<+>), (<->) , (<*>) ,(<.>), (***),vecLength , normalize ) where

import Prelude hiding ( (<*>) )

type Vec3 = (Float,Float,Float)


(<+>) :: Vec3 -> Vec3 -> Vec3
(<+>) (ax,ay,az) (bx,by,bz) = (ax+bx,ay+by,az+bz)


(<->) :: Vec3 -> Vec3 -> Vec3
(<->) (ax,ay,az) (bx,by,bz) = (ax-bx,ay-by,az-bz)

(<*>) :: Vec3 -> Float -> Vec3
(<*>) (x,y,z) f = (x*f,y*f,z*f)

(<.>) :: Vec3 -> Vec3 -> Float
(<.>) (ax,ay,az) (bx,by,bz) = ax*bx + ay*by + az*bz

(***) :: Vec3 -> Vec3 -> Vec3
(***) (ax,ay,az) (bx,by,bz) = (ax*bx , ay*by , az*bz)

vecLength :: Vec3 -> Float
vecLength v = (v <.> v) ** 0.5

normalize:: Vec3 -> Vec3
normalize v = v <*> (1.0 / vecLength v)