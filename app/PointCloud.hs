-- TODO:
-- less dense in center via rejection sampling
-- no dots closer to each other than a certain value. QuadTree for fast collision?
-- voronoi diagram on points, use vectors not pixels, delaunay triangulation, fortune's algorithm?

import Control.Monad (liftM2)
import Data.List (sort)

import Codec.Picture
  ( PixelRGBA8( .. )
  , Image
  , writePng
  )
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import System.Random.MWC
import System.Random.MWC.Distributions (normal, standard)

picWidth = 2880 :: Int
picHeight = 1440 :: Int
picWidth' = fromIntegral picWidth
picHeight' = fromIntegral picHeight
centerX = picWidth' / 2
centerY = picHeight' / 2
numDots = 5000
gridSize = 12
normalStdDev = 600
dotRadiusSmall = 3
dotRadiusLarge = 6
dotColor = PixelRGBA8 0xB8 0xC8 0xEE 0xFF
bgColor = PixelRGBA8 0xAD 0xBA 0xEA 0xFF

main = do
  g <- createSystemRandom
  dots1 <- genDotsNormal numDots g
  let dots1' = map roundPoint dots1
      dots2 = dotGrid picWidth' picHeight' gridSize
      dots2' = map roundPoint dots2
      dotGeometry = 
        drawDots dotRadiusLarge dots1' ++
        drawDots dotRadiusSmall dots2'
  writePng "pointcloud.png" $ render $ dotGeometry

roundPoint :: RealFloat a => (a, a) -> (a, a)
roundPoint (x, y) = (halfRound x, halfRound y)

halfRound :: RealFloat a => a -> a
-- Round to the nearest 0.5
halfRound x = fromIntegral (round (x - 0.5)) + 0.5

dotGrid :: Double -> Double -> Double -> [(Double, Double)]
dotGrid w h cellSize =
  [
    (realToFrac (x*cellSize), realToFrac (y*cellSize))
    | x <- [0..(w / cellSize)]
    , y <- [0..(h / cellSize)]
  ]

genDotsNormal :: Int -> GenIO -> IO [(Double, Double)]
genDotsNormal n g = sequence . replicate n $ loop
  where
    loop = do
      x <- normal centerX normalStdDev g
      y <- normal centerY normalStdDev g
      return (realToFrac x, realToFrac y)

render :: Geometry geom => [geom] -> Image PixelRGBA8
render dots =
  renderDrawing picWidth picHeight bgColor $
  mapM_ (withTexture(uniformTexture dotColor) . dotStroke) dots
  where
    dotStroke = stroke 1 (JoinMiter 0) (CapStraight 0, CapStraight 0)

drawDots :: Double -> [(Double, Double)] -> [[Primitive]]
drawDots radius positions = [squareDot (V2 x y) radius' | (x,y) <- positions']
  where
    radius' = realToFrac radius
    positions' = map (\(x,y) -> (realToFrac x, realToFrac y)) positions
    squareDot (V2 x y) r = rectangle (V2 (x - r) (y - r)) (2*r) (2*r)
