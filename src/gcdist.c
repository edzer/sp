#define USING_R 1
/*  Copyright by Roger Bivand (C) 2005-2009  */

#include "sp.h"

#ifdef USING_R
# define POWDI(x,i) R_pow_di(x,i)
#else
# include <math.h>
# define POWDI(x,i) pow(x,i)
/*# define pythag(a,b) sqrt(a*a+b*b)*/
#endif

void sp_dists(double *u, double *v, double *uout, double *vout, 
		int *n, double *dists, int *lonlat) {
	int N = *n, j;
	double gc[1];
		
	if (lonlat[0] == 0) {
		for (j = 0; j < N; j++) 
			dists[j] = hypot(u[j] - uout[0], v[j] - vout[0]);
	} else {
		for (j = 0; j < N; j++) {
			sp_gcdist(u+j, uout, v+j, vout, gc);
			dists[j] = gc[0];
		}
	}
}

void sp_dists_NN(double *u1, double *v1, double *u2, double *v2, 
		int *n, double *dists, int *lonlat) {
	int N = *n, j;
	double gc[1];
		
	if (lonlat[0] == 0)
		for (j = 0; j < N; j++) 
			dists[j] = hypot(u1[j] - u2[j], v1[j] - v2[j]);
	else {
		for (j = 0; j < N; j++) {
			sp_gcdist(u1+j, u2+j, v1+j, v2+j, gc);
			dists[j] = gc[0];
		}
	}
}

void sp_lengths(double *u, double *v, int *n, double *lengths, int *lonlat) {
	int N = *n, j;
	double gc[1];
        if (N < 2) Rf_error("N less than 2");
		
	if (lonlat[0] == 0)
		for (j=0; j < N-1; j++) 
			lengths[j] = hypot(u[j]-u[j+1], v[j]-v[j+1]);
	else {
		for (j=0; j < N-1; j++) {
			sp_gcdist(u+j, u+j+1, v+j, v+j+1, gc);
			lengths[j] = gc[0];
		}
	}

}

/* http://en.wikipedia.org/wiki/World_Geodetic_System#A_new_World_Geodetic_System:_WGS_84 */

void sp_gcdist(double *lon1, double *lon2, double *lat1, double *lat2, 
		double *dist) {
	
    double F, G, L, sinG2, cosG2, sinF2, cosF2, sinL2, cosL2, S, C;
    double w, R, a, f, D, H1, H2;
    double lat1R, lat2R, lon1R, lon2R, DE2RA;
    
    DE2RA = M_PI/180;
    a = 6378.137;              /* WGS-84 equatorial radius in km */
    f = 1.0/298.257223563;     /* WGS-84 ellipsoid flattening factor */
    
    if (fabs(lat1[0] - lat2[0]) < DBL_EPSILON) {
        if (fabs(fmod(lon1[0] - lon2[0], 360.0)) < DBL_EPSILON) {
            dist[0] = 0.0;
            return;
        }
    }
    lat1R = lat1[0]*DE2RA;
    lat2R = lat2[0]*DE2RA;
    lon1R = lon1[0]*DE2RA;
    lon2R = lon2[0]*DE2RA;
    
    F = ( lat1R + lat2R )/2.0;
    G = ( lat1R - lat2R )/2.0;
    L = ( lon1R - lon2R )/2.0;

	/*
    printf("%g %g %g %g; %g %g %g\n",  *lon1, *lon2, *lat1, *lat2, F, G, L);
	*/

    sinG2 = POWDI( sin( G ), 2 );
    cosG2 = POWDI( cos( G ), 2 );
    sinF2 = POWDI( sin( F ), 2 );
    cosF2 = POWDI( cos( F ), 2 );
    sinL2 = POWDI( sin( L ), 2 );
    cosL2 = POWDI( cos( L ), 2 );

    S = sinG2*cosL2 + cosF2*sinL2;
    C = cosG2*cosL2 + sinF2*sinL2;

    w = atan( sqrt( S/C ) );
    R = sqrt( S*C )/w;

    D = 2*w*a;
    H1 = ( 3*R - 1 )/( 2*C );
    H2 = ( 3*R + 1 )/( 2*S );

    dist[0] = D*( 1 + f*H1*sinF2*cosG2 - f*H2*cosF2*sinG2 ); 

}

