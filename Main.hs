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

fps :: Int
fps =
    30

randomRange :: (Random a, RandomGen g) => (a, a) -> g -> Int -> ([a], g)
randomRange _ g 0 = ([], g)
randomRange (lo, hi) g n =
    let (a, g')   = randomR (lo, hi) g
        (as, g'') = randomRange (lo, hi) g' (n - 1)
    in (a:as, g'')

randomInBox :: (RandomGen g) => g -> Float -> Float -> Float -> Float -> Int -> ([Point2], g)
randomInBox g xmin xmax ymin ymax n =
    (zipWith Point2 xs ys, g'')
        where (xs, g')   = randomRange (xmin, xmax) g n
              (ys, g'')  = randomRange (ymin, ymax) g' n

data World = World { points :: [Point2]
                   , tri    :: [(Int, Int, Int)]
                   , hull   :: [Int]
                   , gen    :: StdGen
                   }

worldToPicture :: World -> Picture
worldToPicture (World ps t h _) =
    pictures $ [ pictures $ map drawPoint ps
               , drawTriangulation ps t
               , drawConvexHull ps h
               ]

drawPoint :: Point2 -> Picture
drawPoint (Point2 px py) =
    translate px py $ color red $ circle 5

drawConvexHull :: [Point2] -> [Int] -> Picture
drawConvexHull ps =
    color blue . lineLoop . map (\i -> pointToTuple (vec V.! i))
        where vec = V.fromList ps
              pointToTuple (Point2 px py) = (px, py)

drawTriangle :: Point2 -> Point2 -> Point2 -> Picture
drawTriangle a b c =
    color green $ lineLoop [pointToTuple a, pointToTuple b, pointToTuple c]
        where pointToTuple (Point2 px py) = (px, py)

drawTriangulation :: [Point2] -> [(Int, Int, Int)] -> Picture
drawTriangulation ps =
    pictures . map (\(ia, ib, ic) -> drawTriangle  (vec V.! ia) (vec V.! ib) (vec V.! ic))
        where vec = V.fromList ps

handleInput :: Event -> World -> World

-- Clear screen
handleInput (EventKey (Char 'c') Down _ _) (World _  _ _ g) =
    World [] [] [] g

-- 2D Hull
handleInput (EventKey (Char 'h') Down _ _) (World ps t _ g) =
    World ps t (convex_hull_2 ps) g

-- 2D Delaunay
handleInput (EventKey (Char 't') Down _ _) (World ps _ h g) =
    World ps (delaunay_2 ps) h g

-- Add 100 random points
handleInput (EventKey (Char 'r') Down _ _) (World ps t h g) =
    World (ps ++ newPs) t h g'
        where (newPs, g') = randomInBox g (-sizeBox) (sizeBox) (-sizeBox) (sizeBox) 100

handleInput _ w =
    w

stepWorld :: Float -> World -> World
stepWorld _ w =
    w

main :: IO ()
main = do
    seed <- getStdGen
    let initialWorld = World { points = []
                             , tri = []
                             , hull = []
                             , gen = seed
                             }
    play (InWindow "CGAL + Haskell" (sizeWindow, sizeWindow) (0, 0))
         black
         fps
         initialWorld
         worldToPicture
         handleInput
         stepWorld

