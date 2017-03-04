#ifndef CGAL_PRELUDE_H
#define CGAL_PRELUDE_H

typedef struct {
    float x;
    float y;
} vec2;

#ifdef __cplusplus

#include <CGAL/Simple_cartesian.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Delaunay_triangulation_2.h>
#include <CGAL/Triangulation_vertex_base_with_info_2.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel Kernel;
/* typedef CGAL::Simple_cartesian<double> Kernel; */

typedef Kernel::FT FT;
typedef Kernel::Point_2 Point_2;

extern "C" {
    int* delaunay_2 (vec2 points[], int n_points, int* n_triangles);
}

#endif /* __cplusplus */

#endif /* CGAL_PRELUDE_H */
