#define USING_R 1
/*	Modified 24 May 2005 Roger S. Bivand for maptools
	Written by Joseph O'Rourke
	orourke@cs.smith.edu
	October 27, 1995

	Computes the centroid (center of gravity) of an arbitrary
	simple polygon via a weighted sum of signed triangle areas,
	weighted by the centroid of each triangle.
	Reads x,y coordinates from stdin.  
	NB: Assumes points are entered in ccw order!  
	E.g., input for square:
		0	0
		10	0
		10	10
		0	10
	This solves Exercise 12, p.47, of my text,
	Computational Geometry in C.  See the book for an explanation
	of why this works. Follow links from
		http://cs.smith.edu/~orourke/

*/

#ifndef USING_R
# define R_alloc(n,size) S_alloc(n,size,S_evaluator)
#endif

#include "sp.h"


/*#define PMAX    1000     	 Max # of pts in polygon */
/* typedef tPointd *tPolygond; */ /* type double polygon */


void	spRFindCG( int *n, double *x, double *y, double *xc, double *yc, 
		double *area ) {

	int i, nn;
	tPointd *P;
	tPointd CG;
	double Areasum2;
	nn = n[0];
	P = (tPointd *) R_alloc((size_t) nn, sizeof(tPointd));
	for (i=0; i<nn; i++) {
		P[i][0] = x[i];
		P[i][1] = y[i];
	}
	FindCG(nn, P, CG, &Areasum2);
	xc[0] = CG[0];
	yc[0] = CG[1];
	area[0] = Areasum2/2;
	return;
}

/*      
        Returns the cg in CG.  Computes the weighted sum of
	each triangle's area times its centroid.  Twice area
	and three times centroid is used to avoid division
	until the last moment.
*/

