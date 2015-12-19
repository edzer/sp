# include "sp.h"
# include <Rinternals.h>

#define ROFFSET 1


int pipbb(double pt1, double pt2, double *bbs);

int between(double x, double low, double up); 

SEXP pointsInBox(const SEXP lb, const SEXP px, const SEXP py);

SEXP tList(const SEXP nl, const SEXP m);


SEXP tList(const SEXP nl, const SEXP m0) {
    int n=length(nl), m=INTEGER_POINTER(m0)[0], i, ii, j, jj, *k, pc=0;
    SEXP res;
    PROTECT(res = NEW_LIST(m)); pc++;
    k = (int *) R_alloc((size_t) m, sizeof(int));
    for (j=0; j<m; j++) k[j] = 0;
    for (i=0; i<n; i++) {
        ii = length(VECTOR_ELT(nl, i));
        if (ii > 0) {
            for (j=0; j<ii; j++) {
                jj = INTEGER_POINTER(VECTOR_ELT(nl, i))[j] - ROFFSET;
                if (jj < 0 || jj >= m) error("invalid indices");
                k[jj]++;
            }
        }
    }
    for (j=0; j<m; j++) SET_VECTOR_ELT(res, j, NEW_INTEGER(k[j]));
    for (j=0; j<m; j++) k[j] = 0;
    for (i=0; i<n; i++) {
        ii = length(VECTOR_ELT(nl, i));
        if (ii > 0) {
            for (j=0; j<ii; j++) {
                jj = INTEGER_POINTER(VECTOR_ELT(nl, i))[j] - ROFFSET;
                INTEGER_POINTER(VECTOR_ELT(res, jj))[k[jj]] = i + ROFFSET;
                k[jj]++;
            }
        }
    }
    UNPROTECT(pc); 
    return(res);
}

SEXP pointsInBox(const SEXP lb, const SEXP px, const SEXP py) {
    int n=length(px), m=length(lb), i, j, jj, *k, sk, pc=0;
    double *x, ppx, ppy;
    SEXP res, px1, py1, lb1;

    PROTECT(px1 = duplicate(px)); pc++;
    PROTECT(py1 = duplicate(py)); pc++;
    PROTECT(lb1 = duplicate(lb)); pc++;
    PROTECT(res = NEW_LIST(n)); pc++;
    x = (double *) R_alloc((size_t) (m*4), sizeof(double));
    k = (int *) R_alloc((size_t) m, sizeof(int));
    for (i=0; i<m; i++) {
        for (j=0; j<4; j++) x[(i*4)+j] = NUMERIC_POINTER(VECTOR_ELT(lb1, i))[j];
    }
    for (i=0; i<n; i++) {
        ppx = NUMERIC_POINTER(px1)[i];
        ppy = NUMERIC_POINTER(py1)[i];
        for (j=0; j<m; j++) k[j] = 0;
        for (j=0; j<m; j++) k[j] = pipbb(ppx, ppy, &x[j*4]);
        sk=0;
        for(j=0; j<m; j++) sk += k[j];
        SET_VECTOR_ELT(res, i, NEW_INTEGER(sk));
        jj=0;
        for(j=0; j<m; j++) {
            if (k[j] == 1) {
                INTEGER_POINTER(VECTOR_ELT(res, i))[jj] = j + ROFFSET;
                jj++;
            }
        }
    }    
    UNPROTECT(pc); 
    return(res);
}

int between(double x, double low, double up) {
	if (x >= low && x <= up) return(1);
	else return(0);
}

int pipbb(double pt1, double pt2, double *bbs) {
	if ((between(pt1, bbs[0], bbs[2]) == 1) && 
		(between(pt2, bbs[1], bbs[3]) == 1)) return(1);
	else return(0);
} 


