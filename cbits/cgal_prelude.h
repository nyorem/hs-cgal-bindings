#ifndef CGAL_PRELUDE_H
#define CGAL_PRELUDE_H

typedef struct {
    float x;
    float y;
} vec2;

typedef struct {
    float x;
    float y;
    float z;
} vec3;

#ifdef __cplusplus

#include <CGAL/Simple_cartesian.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>

#include <CGAL/convex_hull_2.h>
#include <CGAL/Delaunay_triangulation_2.h>
#include <CGAL/Triangulation_vertex_base_with_info_2.h>

#include <CGAL/Delaunay_triangulation_3.h>
#include <CGAL/Triangulation_vertex_base_with_info_3.h>

// Kernel
typedef CGAL::Exact_predicates_inexact_constructions_kernel Kernel;
/* typedef CGAL::Simple_cartesian<double> Kernel; */

extern "C" {
    // Convex hulls
    // 2D
    int* convex_hull_2 (vec2 points[], int n_points, int* size_hull);

    // Triangulations
    // 2D Delaunay
    int* delaunay_2 (vec2 points[], int n_points, int* n_triangles);
}

#endif /* __cplusplus */

#endif /* CGAL_PRELUDE_H */
