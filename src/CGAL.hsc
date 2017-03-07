{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module CGAL where

import Foreign
import Foreign.C
import System.IO.Unsafe

#include "cgal_prelude.h"

-- Types

data Point2 = Point2 Float Float
    deriving (Show)

instance Storable Point2 where
    sizeOf _ = #{size vec2}

    alignment _ = alignment (undefined :: Float)

    poke p (Point2 x y) = do
        #{poke vec2, x} p $ x
        #{poke vec2, y} p $ y

    peek p =
        Point2 <$> #{peek vec2, x} p <*> #{peek vec2, y} p

data Point3 = Point3 Float Float Float
    deriving (Show)

instance Storable Point3 where
    sizeOf _ = #{size vec3}

    alignment _ = alignment (undefined :: Float)

    poke p (Point3 x y z) = do
        #{poke vec3, x} p $ x
        #{poke vec3, y} p $ y
        #{poke vec3, z} p $ z

    peek p =
        Point3 <$> #{peek vec3, x} p <*> #{peek vec3, y} p <*> #{peek vec3, z} p

-- Misc
groupByN :: Int -> [a] -> [[a]]
groupByN n xs
    | length xs < n = [xs]
    | otherwise = (take n xs) : groupByN n (drop n xs)

-- Convex hulls
-- 2D
foreign import ccall unsafe "convex_hull_2" internal_convex_hull_2 :: Ptr Point2 -> CInt -> Ptr CInt -> IO (Ptr CInt)

convex_hull_2 :: [Point2] -> [Int]
convex_hull_2 ps = unsafePerformIO $ do
    (final_ptr_hull, final_ptr_n_points) <- withArray ps $ \ps_ptr -> do
        (ptr_hull, ptr_n_points) <- alloca $ \n_points_ptr -> do
            c_hull <- internal_convex_hull_2 ps_ptr (fromIntegral $ length ps) n_points_ptr
            return (c_hull, n_points_ptr)
        return (ptr_hull, ptr_n_points)
    n_points <- peek final_ptr_n_points
    hull <- peekArray (fromIntegral n_points) final_ptr_hull
    free final_ptr_hull
    return $ map fromIntegral hull

-- Triangulations
-- 2D Delaunay
foreign import ccall unsafe "delaunay_2" internal_delaunay_2 :: Ptr Point2 -> CInt -> Ptr CInt -> IO (Ptr CInt)

delaunay_2 :: [Point2] -> [(Int, Int, Int)]
delaunay_2 ps = unsafePerformIO $ do
    (final_ptr_tri, final_ptr_n_triangles) <- withArray ps $ \ps_ptr -> do
        (ptr_tri, ptr_n_triangles) <- alloca $ \n_tri_ptr -> do
            c_tri <- internal_delaunay_2 ps_ptr (fromIntegral $ length ps) n_tri_ptr
            return (c_tri, n_tri_ptr)
        return (ptr_tri, ptr_n_triangles)
    n_tri <- peek final_ptr_n_triangles
    tri <- peekArray (fromIntegral (3 * n_tri)) final_ptr_tri
    free final_ptr_tri
    return $ map (\[a, b, c] -> (a, b, c)) $ init $ groupByN 3 $ map fromIntegral tri

