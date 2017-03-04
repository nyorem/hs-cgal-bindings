{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module CGAL where

import Foreign
import Foreign.C
import System.IO.Unsafe

#include "cgal_prelude.h"

foreign import ccall unsafe "delaunay_2" internal_delaunay_2 :: Ptr Point2 -> CInt -> Ptr CInt -> IO (Ptr CInt)

data Point2 = Point2 { x :: Float , y :: Float } deriving (Show)

instance Storable Point2 where
    sizeOf _ = #{size vec2}

    alignment _ = alignment (undefined :: CInt)

    poke p vec2 = do
        #{poke vec2, x} p $ x vec2
        #{poke vec2, y} p $ y vec2

    peek p =
        Point2 <$> #{peek vec2, x} p <*> #{peek vec2, y} p

groupByN :: Int -> [a] -> [[a]]
groupByN n xs
    | length xs < n = [xs]
    | otherwise = (take n xs) : groupByN n (drop n xs)

delaunay_2 :: [Point2] -> [(Int, Int, Int)]
delaunay_2 ps = unsafePerformIO $ do
    let n_points = fromIntegral (length ps) :: CInt
    (final_ptr_tri, final_ptr_n_triangles) <- withArray ps $ \ps_ptr -> do
        (ptr_tri, ptr_n_triangles) <- alloca $ \n_tri_ptr -> do
            c_tri <- internal_delaunay_2 ps_ptr n_points n_tri_ptr
            return (c_tri, n_tri_ptr)
        return (ptr_tri, ptr_n_triangles)
    n_tri <- peek final_ptr_n_triangles
    tri <- peekArray (fromIntegral (3 * n_tri)) final_ptr_tri
    free final_ptr_tri
    return $ map (\[a, b, c] -> (a, b, c)) $ init $ groupByN 3 $ map fromIntegral tri

