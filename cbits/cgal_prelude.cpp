#include "cgal_prelude.h"

#ifdef __cplusplus

#include <iostream>
#include <vector>

extern "C" int* delaunay_2 (vec2 points[], int n_points, int* n_triangles) {
    typedef CGAL::Triangulation_vertex_base_with_info_2<int, Kernel> Vb;
    typedef CGAL::Triangulation_data_structure_2<Vb> Tds;
    typedef CGAL::Delaunay_triangulation_2<Kernel, Tds> Delaunay_triangulation_2;

    std::vector<std::pair<Point_2, int>> points_vec;
    for (int i = 0; i < n_points; ++i) {
        std::pair<Point_2, int> p_info;
        p_info.first = Point_2(points[i].x, points[i].y);
        p_info.second = i;
        points_vec.push_back(p_info);
    }

    Delaunay_triangulation_2 dt(points_vec.begin(), points_vec.end());

    *n_triangles = dt.number_of_faces();
    int *triangles = new int[3 * *n_triangles];

    int it = 0;
    for (Delaunay_triangulation_2::Finite_faces_iterator fit = dt.finite_faces_begin();
         fit != dt.finite_faces_end();
         ++fit) {
        Delaunay_triangulation_2::Triangle t = dt.triangle(fit);
        triangles[3 * it + 0] = fit->vertex(0)->info();
        triangles[3 * it + 1] = fit->vertex(1)->info();
        triangles[3 * it + 2] = fit->vertex(2)->info();
        it++;
    }

    return triangles;
}

#endif

