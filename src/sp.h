#ifndef R_SP_H
#define R_SP_H

#ifndef R_NO_REMAP
# define R_NO_REMAP
#endif

#include <R.h>
/* RSB 091203 */
#include <Rdefines.h>
#define R_OFFSET 1
#include <Rinternals.h>
#include <Rmath.h>

/* from insiders.c 

int pipbb(double pt1, double pt2, double *bbs);
int between(double x, double low, double up); 
SEXP insiders(SEXP n1, SEXP bbs); */

/* from pip.c */

#ifndef MIN
# define MIN(a,b) ((a)>(b)?(b):(a))
#endif
#ifndef MAX
# define MAX(a,b) ((a)>(b)?(a):(b))
#endif

#define BUFSIZE 8192

/* polygon structs: */
typedef struct {
	double		x, y;
} PLOT_POINT;

typedef struct {
	PLOT_POINT	min, max;
} MBR;

typedef struct polygon {
	MBR mbr;
	int lines;
	PLOT_POINT	*p;
    int close; /* 1 - is closed polygon */
} POLYGON;

void setup_poly_minmax(POLYGON *pl);
char InPoly(PLOT_POINT q, POLYGON *Poly);
SEXP R_point_in_polygon_sp(SEXP px, SEXP py, SEXP polx, SEXP poly); /* point.in.polygon.R */
void sarea(double *heights, int *nx, int *ny, double *w, double *h, 
	double *sa, int *bycell); /* surfaceArea.R */
void spRFindCG( int *n, double *x, double *y, double *xc, double *yc, 
		double *area ); /* SpatialPolygons-internals.R */
void sp_gcdist(double *lon1, double *lon2, double *lat1, double *lat2, 
		double *dist);
void sp_dists(double *u, double *v, double *uout, double *vout, 
		int *n, double *dists, int *lonlat); /* spdists.R */
void sp_dists_NN(double *u1, double *v1, double *u2, double *v2,
        int *n, double *dists, int *lonlat); /* spdists.R */
void sp_lengths(double *u, double *v, int *n, double *lengths, int *lonlat); /* SpatialLines-methods.R */
SEXP sp_zerodist(SEXP pp, SEXP pncol, SEXP zero, SEXP lonlat, SEXP mcmp); /* zerodist.R */
SEXP sp_duplicates(SEXP pp, SEXP pncol, SEXP zero, SEXP lonlat, SEXP mcmp); /* zerodist.R */
SEXP pointsInBox(SEXP lb, SEXP px, SEXP py); /* point.in.polygon.R */
SEXP tList(SEXP nl, SEXP m); /* point.in.polygon.R */

/* RSB 091203 */

#define DIM     2               /* Dimension of points */
typedef double  tPointd[DIM];   /* type double point */

double  Area2(const tPointd a, const tPointd b, const tPointd c);
void    FindCG(int n, tPointd *P, tPointd CG, double *Areasum2);
void    Centroid3(const tPointd p1, const tPointd p2, 
	const tPointd p3, tPointd c);
void spRFindCG_c(const SEXP n, const SEXP coords, 
	double *xc, double *yc, double *area );
void comm2comment(char *buf, int bufsiz, int *comm, int nps);

SEXP Polygon_c(const SEXP coords, const SEXP n, const SEXP hole); /* SpatialPolygons-methods.R */
SEXP Polygons_c(const SEXP pls, const SEXP ID); /* SpatialPolygons-methods.R */
SEXP SpatialPolygons_c(const SEXP pls, const SEXP pO, const SEXP p4s); /* SpatialPolygons-methods.R */
SEXP bboxCalcR_c(const SEXP pls); /* SpatialPolygonsDataFrame-methods.R */
SEXP Polygon_validate_c(const SEXP obj); /* Class-SpatialPolygons.R */
SEXP Polygons_validate_c(const SEXP obj); /* Class-SpatialPolygons.R */
SEXP SpatialPolygons_validate_c(const SEXP obj); /* Class-SpatialPolygons.R */
SEXP SpatialPolygons_getIDs_c(const SEXP obj); /* chfids.R Class-SpatialPolygons.R SpatialPolygonsDataFrame-methods.R */
SEXP SpatialPolygons_plotOrder_c(const SEXP pls); /* SpatialPolygonsDataFrame-methods.R SpatialPolygons-methods.R */
SEXP comment2comm(const SEXP obj);
// SEXP sp_linkingTo_version(void);
#endif

