import Haste
import Haste.DOM
import Haste.Graphics.Canvas

import Control.Applicative
import Data.Complex

sch :: Double -> Complex Double -> Complex Double
sch α' z = let
    α = α' :+ 0
    p = sqrt $ (1 - α) / α
    in
        (z + (2*p))**(1-α) * (z - 2/p)**α

schL :: [Double] -> [Complex Double]
schL = scanl (flip sch) (0:+0)

drawPath :: Canvas -> [Complex Double] -> IO ()
drawPath canv =
    renderOnTop canv . translate (500,350) . stroke . path . map complexToPoint

complexToPoint :: Complex Double -> Point
complexToPoint (x :+ y) = (x,y)

randomWalk :: Double -> Seed -> [Double]
randomWalk w = scanl (+) 1 . map ((*w ) . fromIntegral . subtract 1 . (*2))
    . randomRs (0, 1::Int)

main = do
    let w = 0.02 :: Double
    Just canv <- getCanvasById "canv0"
    ps <- schL . map (*0.90).  take 1000 . randomWalk w <$> newSeed
    print ps
    drawPath canv  ps
