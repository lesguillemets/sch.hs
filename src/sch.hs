import Haste
import Haste.DOM
import Haste.Graphics.Canvas

import Control.Applicative
import Data.Complex
import Data.List
import Data.Function (on)


sch :: Double -> Complex Double -> Complex Double
sch α' z = let
    α = α' :+ 0
    p = sqrt $ (1 - α) / α
    th = 0.0001
    in
        if abs α' > th && abs (1-α') > th
            then (z + (2*p))**(1-α) * (z - 2/p)**α
            else z

schL :: [Double] -> [Complex Double]
schL = scanl (flip sch) (0:+0)

drawPath :: Canvas -> [Complex Double] -> IO ()
drawPath canv =
    renderOnTop canv . translate (0,350) . stroke . path . map complexToPoint

complexToPoint :: Complex Double -> Point
complexToPoint (x :+ y) = (x,y)

randomWalk :: Double -> Seed -> [Double]
randomWalk w = scanl (+) 0 . map ((*w ) . fromIntegral . subtract 1 . (*2))
    . randomRs (0, 1::Int)

main = do
    let w = 0.0002 :: Double
    Just canv <- getCanvasById "canv0"
    s <- newSeed
    let rw = randomWalk w s
        ps = schL $  take 10000 rw
        m = maximumBy (compare `on` magnitude) ps
        f = map (* ((500/magnitude m):+0)) ps
    print m
    drawPath canv f
