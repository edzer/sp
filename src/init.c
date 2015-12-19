#include <R.h>
#include <Rinternals.h>
#include "sp.h"

#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[]  = {
/*    {"pipbb", (DL_FUNC) &pipbb, 3},
    {"between", (DL_FUNC) &between, 3}, */
    {"setup_poly_minmax", (DL_FUNC) &setup_poly_minmax, 1},
    {"InPoly", (DL_FUNC) &InPoly, 2},
    {"sarea", (DL_FUNC) &sarea, 7},
    {"spRFindCG", (DL_FUNC) &spRFindCG, 6},
    {"sp_gcdist", (DL_FUNC) &sp_gcdist, 5},
    {"sp_dists", (DL_FUNC) &sp_dists, 7},
    {"sp_dists_NN", (DL_FUNC) &sp_dists_NN, 7},
    {"sp_lengths", (DL_FUNC) &sp_lengths, 5},
/* RSB 091203 */
    {"spRFindCG_c", (DL_FUNC) &spRFindCG_c, 5},
    {"comm2comment", (DL_FUNC) &comm2comment, 4},
    {NULL, NULL, 0}
};

static R_CallMethodDef CallEntries[] = {
/*    {"insiders", (DL_FUNC) &insiders, 2}, */
    {"R_point_in_polygon_sp", (DL_FUNC) &R_point_in_polygon_sp, 4},
	{"sp_zerodist", (DL_FUNC) &sp_zerodist, 5},
	{"sp_duplicates", (DL_FUNC) &sp_duplicates, 5},
    {"pointsInBox", (DL_FUNC) &pointsInBox, 3},
    {"tList", (DL_FUNC) &tList, 2},
/* RSB 091203 */
    {"Polygon_c", (DL_FUNC) &Polygon_c, 3},
    {"Polygons_c", (DL_FUNC) &Polygons_c, 2},
    {"SpatialPolygons_c", (DL_FUNC) &SpatialPolygons_c, 3},
    {"bboxCalcR_c", (DL_FUNC) &bboxCalcR_c, 1},
    {"Polygon_validate_c", (DL_FUNC) &Polygon_validate_c, 1},
    {"Polygons_validate_c", (DL_FUNC) &Polygons_validate_c, 1},
    {"SpatialPolygons_validate_c", (DL_FUNC) &SpatialPolygons_validate_c, 1},
    {"SpatialPolygons_getIDs_c", (DL_FUNC) &SpatialPolygons_getIDs_c, 1},
    {"SpatialPolygons_plotOrder_c", (DL_FUNC) &SpatialPolygons_plotOrder_c, 1},
    {"comment2comm", (DL_FUNC) &comment2comm, 1},
    {"sp_linkingTo_version", (DL_FUNC) &sp_linkingTo_version, 0},
    {NULL, NULL, 0}
};


void 
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_sp(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

}

