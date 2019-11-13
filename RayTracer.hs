module RayTracer(main) where

import Prelude hiding ( (<*>) )
import Image
import Math
import Rand

type Sphere = (Vec3,Float,Vec3)  -- (center,radius,color)
type Ray = (Vec3,Vec3) -- (Origin, Direction)
data HitResult = Hit Sphere Vec3 Float| NoHit -- (HitSphere, HitPos,distance)
type Scene = [Sphere]

map2 :: (a->b) -> [[a]] -> [[b]]
map2 f = map (map f)


hitSphere :: Sphere -> Ray -> HitResult
hitSphere  (center,r,color)  (o,v) = 
    let oc = o <-> center in
    let a = v <.> v in
    let b = ( oc <.> v ) * 2 in
    let c = ( oc<.> oc  ) - r*r in
    let delta = b*b - 4*a*c in
    if delta >= 0 then
        let t1 = (-b + delta **0.5) / (2*a) in
        let t2 = (-b - delta **0.5) / (2*a) in
        let hit1 = o <+> ( v <*> t1) in
        let hit2 = o <+> ( v <*> t2) in
        let epsilon = 1e-3 in
        if t1 <=  epsilon then NoHit
        else if t2 <= epsilon then Hit (center,r,color) hit1 t1
        else Hit (center,r,color) hit2 t2
    else
        NoHit


closer :: HitResult -> HitResult -> HitResult
closer x NoHit = x
closer NoHit x = x
closer (Hit s1 p1 t1) (Hit s2 p2 t2) = 
    if t1 < t2 then 
        (Hit s1 p1 t1)
    else
        (Hit s2 p2 t2)

hitScene :: Scene -> Ray-> HitResult
hitScene [] ray = NoHit
hitScene (s:ss) ray = 
    let hit1 = hitSphere s ray in
    let hit2 = hitScene ss ray in
    closer hit1 hit2

getNormal :: HitResult -> Vec3
getNormal NoHit = error "should be called"
getNormal (Hit s pos _) = 
    let (center, r, color) = s in
    normalize ( pos <-> center )

shadeRay :: Int -> Scene -> Ray -> Color
shadeRay level scene (o,v) = 
    if level > 5 then (0,0,0) else
    shade (hitScene scene (o,v))
    where   up = 125.0 + 125.0 * (v <.> (0.0,1.0,0.0))
            shade NoHit = (up,up,up) 
            shade (Hit sphere pos t) = 
                let (center,radius,color) = sphere in
                let hitNormal = getNormal (Hit sphere pos t) in
                let newDir = hitNormal <+> randVec hitNormal in
                let newRay = (pos, normalize newDir) in
                color *** (shadeRay (level+1)  scene newRay) <*> 0.7
    

genRays :: Vec3 -> Vec3 -> Vec3 -> (Int,Int) ->[[Ray]]
genRays eye topLeft bottomRight (width,height) = 
    let (x0,y1,z) = topLeft in
    let (x1,y0,_) = bottomRight in
    let pixelWidth = (x1-x0) / fromIntegral width in
    let pixelHeight = (y1-y0) / fromIntegral height in
    let row y = [ ( eye, (x0 + fromIntegral i *pixelWidth , y , z) <-> eye) |i<-[0..width]] in
    let unnormalized = reverse [row (y0 + fromIntegral i *pixelHeight) | i<-[0..height] ] in
    let normalizeRay (o,v) = (o, normalize v) in
    map2 normalizeRay unnormalized



render :: Scene -> Image
render scene =
    let eye = (0,0,0) in
    let z = 10 in
    let topLeft = (-10,5,z) in
    let bottomRight = (10,-5,z) in
    let rays = genRays eye topLeft bottomRight (2000,1000) in
    let results = map2 ( shadeRay 0 scene) rays in
    results

scene :: Scene
scene =  [ ((0,0,30),10,(1,1,0)) ,((0,-100,30),90,(0,1,1)) , ((8,-8,22),3,(1,0,1))] 

testImage :: Image
testImage =  [ [ (i,j,0) | i <- [0..255] ] | j<-[0..255] ]

main = produceImage "image.ppm" $ render scene