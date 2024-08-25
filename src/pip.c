# include <R.h>
# include <Rinternals.h>

#include "sp.h"

SEXP R_point_in_polygon_sp(const SEXP px, const SEXP py, const SEXP polx, 
		const SEXP poly) {
	int i, pc=0;
	PLOT_POINT p;
	POLYGON pol;
	SEXP ret, px1, py1, polx1, poly1;

	if (MAYBE_REFERENCED(px)) {
		PROTECT(px1 = Rf_duplicate(px));
		pc++;
	} else
		px1 = px;
	if (MAYBE_REFERENCED(py)) {
		PROTECT(py1 = Rf_duplicate(py));
		pc++;
	} else
		py1 = py;
	if (MAYBE_REFERENCED(polx)) {
		PROTECT(polx1 = Rf_duplicate(polx));
		pc++;
	} else
		polx1 = polx;
	if (MAYBE_REFERENCED(poly)) {
		PROTECT(poly1 = Rf_duplicate(poly));
		pc++;
	} else
		poly1 = poly;

	pol.lines = LENGTH(polx); /* check later that first == last */
	pol.p = (PLOT_POINT *) R_alloc((size_t) pol.lines, sizeof(PLOT_POINT)); 
	/* transient; will be freed by R; freed by R on user interrupt */
	for (i = 0; i < LENGTH(polx); i++) {
		pol.p[i].x = NUMERIC_POINTER(polx)[i];
		pol.p[i].y = NUMERIC_POINTER(poly)[i];
	}
    pol.close = (pol.p[0].x == pol.p[pol.lines - 1].x && 
			pol.p[0].y == pol.p[pol.lines - 1].y);
	setup_poly_minmax(&pol);

	PROTECT(ret = NEW_INTEGER(LENGTH(px))); pc++;
	for (i = 0; i < LENGTH(px); i++) {
		p.x = NUMERIC_POINTER(px)[i];
		p.y = NUMERIC_POINTER(py)[i];
/*
For each query point q, InPoly returns one of four char's:
	i : q is strictly interior to P
	o : q is strictly exterior to P
	v : q is a vertex of P
	e : q lies on the relative interior of an edge of P
*/
		switch (InPoly(p, &pol)) {
			case 'i': INTEGER_POINTER(ret)[i] = 1; break;
			case 'o': INTEGER_POINTER(ret)[i] = 0; break;
			case 'v': INTEGER_POINTER(ret)[i] = 3; break;
			case 'e': INTEGER_POINTER(ret)[i] = 2; break;
			default: INTEGER_POINTER(ret)[i] = -1; break;
		}
	}
	UNPROTECT(pc);
	return(ret);
}

void setup_poly_minmax(POLYGON *pl) {
    int i, n=pl->lines;
    double minx,maxx,miny,maxy;
    
    minx=miny=DBL_MAX;
    maxx=maxy=-DBL_MAX;
    
    for (i=0;i<n;i++) {
        minx = MIN(minx, pl->p[i].x);
        miny = MIN(miny, pl->p[i].y);
        maxx = MAX(maxx, pl->p[i].x);
        maxy = MAX(maxy, pl->p[i].y);
    }
    pl->mbr.min.x = minx;
    pl->mbr.min.y = miny;
    pl->mbr.max.x = maxx;
    pl->mbr.max.y = maxy;
}

/*
This code is described in "Computational Geometry in C" (Second Edition),
Chapter 7.  It is not written to be comprehensible without the 
explanation in that book.

For each query point q, InPoly returns one of four char's:
	i : q is strictly interior to P
	o : q is strictly exterior to P
	v : q is a vertex of P
	e : q lies on the relative interior of an edge of P
These represent mutually exclusive categories.
For an explanation of the code, see Chapter 7 of 
"Computational Geometry in C (Second Edition)."

Written by Joseph O'Rourke, contributions by Min Xu, June 1997.
Questions to orourke@cs.smith.edu.
--------------------------------------------------------------------
This code is Copyright 1998 by Joseph O'Rourke.  It may be freely 
redistributed in its entirety provided that this copyright notice is 
not removed.
--------------------------------------------------------------------
*/

/*
InPoly returns a char in {i,o,v,e}.  See above for definitions.
*/

char InPoly(PLOT_POINT q, POLYGON *Poly)
{
    int n = Poly->lines;
    PLOT_POINT *P=Poly->p;
    
    int	 i, i1;      /* point index; i1 = i-1 mod n */
    double x;          /* x intersection of e with ray */
    double xx=q.x, yy=q.y;
    int	 Rcross = 0; /* number of right edge/ray crossings */
    int    Lcross = 0; /* number of left edge/ray crossings */

    /* For each edge e=(i-1,i), see if crosses ray. */
    for( i = 0; i < n; i++ ) {
        /* First see if q=(0,0) is a vertex. */
        if (( P[i].x - xx )==0 &&( P[i].y - yy )==0 ) return 'v';
        i1 = ( i + n - 1 ) % n;
        /* printf("e=(%d,%d)\t", i1, i); */
    
        /* if e "straddles" the x-axis... */
        /* The commented-out statement is logically equivalent to the one 
           following. */
        /* if( ( ( P[i].y > 0 ) && ( P[i1].y <= 0 ) ) ||
           ( ( P[i1].y > 0 ) && ( P[i] .y <= 0 ) ) ) { }*/
    
        if( (( P[i].y - yy ) > 0 ) != (( P[i1].y - yy ) > 0 ) ) {
      
            /* e straddles ray, so compute intersection with ray. */
            x = (( P[i].x - xx) *( P[i1].y - yy ) -( P[i1].x - xx ) *( P[i].y - yy )) /
                (P[i1].y - P[i].y );
            /* printf("straddles: x = %g\t", x); */
      
            /* crosses ray if strictly positive intersection. */
            if (x > 0) Rcross++;
        }
        /* printf("Right cross=%d\t", Rcross); */
    
        /* if e straddles the x-axis when reversed... */
        /* if( ( ( P[i] .y < 0 ) && ( P[i1].y >= 0 ) ) ||
           ( ( P[i1].y < 0 ) && ( P[i] .y >= 0 ) ) )  { }*/
    
        if ( (( P[i].y - yy ) < 0 ) != (( P[i1].y - yy ) < 0 ) ) { 
      
            /* e straddles ray, so compute intersection with ray. */
            x = (( P[i].x - xx) *( P[i1].y - yy ) -( P[i1].x - xx ) *( P[i].y - yy )) /
                (P[i1].y - P[i].y);
            /* printf("straddles: x = %g\t", x); */

            /* crosses ray if strictly positive intersection. */
            if (x < 0) Lcross++;
        }
        /* printf("Left cross=%d\n", Lcross); */
    }	
  
    /* q on the edge if left and right cross are not the same parity. */
    if( ( Rcross % 2 ) != (Lcross % 2 ) )
        return 'e';
  
    /* q inside iff an odd number of crossings. */
    if( (Rcross % 2) == 1 )
        return 'i';
    else
		return 'o';
}
