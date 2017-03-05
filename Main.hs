import CGAL
import qualified Data.Vector as V
import Graphics.Gloss.Interface.Pure.Game
import System.Random

sizeWindow :: Int
sizeWindow =
    600

sizeBox :: Float
sizeBox =
    (fromIntegral sizeWindow / 2) - 20

seed :: StdGen
seed =
    mkStdGen 42

fps :: Int
fps =
    30

points0 :: [Point2]
points0 =
    [ Point2 (-sizeBox) (-sizeBox)
    , Point2 sizeBox (-sizeBox)
    , Point2 sizeBox sizeBox
    , Point2 (-sizeBox) sizeBox
    ]

randomRange :: (Random a, RandomGen g) => (a, a) -> g -> Int -> ([a], g)
randomRange (lo, hi) g 0 = ([], g)
randomRange (lo, hi) g n =
    let (a, newG) = randomR (lo, hi) g
        (as, g') = randomRange (lo, hi) newG (n - 1)
    in (a:as, g')

randomInBox :: (RandomGen g) => g -> Float -> Float -> Float -> Float -> Int -> ([Point2], g)
randomInBox g xmin xmax ymin ymax n =
    (zipWith Point2 xs ys, g'')
        where (xs, g') = randomRange (xmin, xmax) g n
              (ys, g'')  = randomRange (ymin, ymax) g' n

data World = World { points :: [Point2]
                   , tri    :: [(Int, Int, Int)]
                   , gen    :: StdGen
                   }

initialWorld :: World
initialWorld =
    World points0 [] seed

worldToPicture :: World -> Picture
worldToPicture (World ps t _) =
    pictures $ [ pictures $ map drawPoint ps
               , drawTriangulation ps t
               ]

drawTriangulation :: [Point2] -> [(Int, Int, Int)] -> Picture
drawTriangulation ps =
    pictures . map (\(ia, ib, ic) -> drawTriangle  (vec V.! ia) (vec V.! ib) (vec V.! ic))
        where vec = V.fromList ps

drawPoint :: Point2 -> Picture
drawPoint (Point2 x y) =
    translate x y $ color red $ circle 5

drawTriangle :: Point2 -> Point2 -> Point2 -> Picture
drawTriangle a b c =
    color green $ lineLoop [pointToTuple a, pointToTuple b, pointToTuple c]
        where pointToTuple (Point2 x y) = (x, y)

handleInput :: Event -> World -> World
handleInput (EventKey (Char 't') Down _ _) w@(World ps t g)
  | null t    = World ps (delaunay_2 ps) g
    | otherwise = w
handleInput (EventKey (Char 'r') Down _ _) (World ps t g) = World (ps ++ newPs) t g'
    where (newPs, g') = randomInBox g (-sizeBox) (sizeBox) (-sizeBox) (sizeBox) 100
handleInput _ w = w

stepWorld :: Float -> World -> World
stepWorld _ w =
    w

main :: IO ()
main = do
    play (InWindow "CGAL + Haskell" (sizeWindow, sizeWindow) (0, 0))
         black
         fps
         initialWorld
         worldToPicture
         handleInput
         stepWorld

