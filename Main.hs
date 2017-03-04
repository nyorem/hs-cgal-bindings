import CGAL

points :: [Point2]
points =
    [ Point2 0 0
    , Point2 1 0
    , Point2 1 1
    , Point2 0 1
    ]

main :: IO ()
main = do
    print $ delaunay_2 points

