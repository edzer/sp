#include <string.h> /* memcmp */

#include <R.h>
#include <Rinternals.h>

#include "sp.h"

int is_zero(double *xi, double *xj, int ncol, int ll, double zerodist2,
		int cmp) {
	int k;
	double d, dist;

	if (zerodist2 <= 0.0) /* check bitwise equality */
		return (memcmp(xi, xj, ncol * sizeof(double)) == 0); 
	/* bit-wise difference: compute dist, and compare to zerodist2 */
	if (ll) {
		sp_gcdist(xi, xj, xi+1, xj+1, &d);
		dist = d * d;
	} else {
		for (k = 0, dist = 0.0; k < ncol; k++) {
			d = (xi[k] - xj[k]);
			dist += d * d;
		}
	}
	return(dist <= zerodist2);
}

SEXP sp_zerodist(SEXP pp, SEXP pncol, SEXP zero, SEXP lonlat, SEXP mcmp) {
	unsigned int i, j, ncol, nrow, nzero = 0, *which = NULL, ll, cmp;
	double **x, zerodist2;
	SEXP ret = NULL;

	ncol = INTEGER_POINTER(pncol)[0];
	ll = INTEGER_POINTER(lonlat)[0];
	cmp = INTEGER_POINTER(mcmp)[0];
	if (ll && ncol != 2)
		error("for longlat data, coordinates should be two-dimensional");
	nrow = LENGTH(pp)/ncol;
	zerodist2 = NUMERIC_POINTER(zero)[0] * NUMERIC_POINTER(zero)[0];
	x = (double **) malloc((size_t) nrow * sizeof(double *));
	if (x == NULL)
		error("could not allocate vector of %u bytes in zerodist",
			nrow * sizeof(double *));
	for (i = 0; i < nrow; i++)
		x[i] = &(NUMERIC_POINTER(pp)[i*ncol]);

	for (i = 0; i < nrow; i++) {
		for (j = 0; j < i; j++) {
			if (is_zero(x[i], x[j], ncol, ll, zerodist2, cmp)) {
				which = (unsigned int *) realloc(which, (size_t) (nzero+2) * sizeof(unsigned int));
				if (which == NULL)
					error("could not allocate vector of %u bytes in zerodist",
						nzero + 2);
				which[nzero] = j; /* lowest */
				which[nzero + 1] = i;
				nzero += 2;
			}
		}
		R_CheckUserInterrupt();
	}
	free(x);
	PROTECT(ret = NEW_INTEGER(nzero));
	for (i = 0; i < nzero; i++)
		INTEGER_POINTER(ret)[i] = which[i];
	if (which != NULL)
		free(which);
	UNPROTECT(1);
	return(ret);
}

SEXP sp_duplicates(SEXP pp, SEXP pncol, SEXP zero, SEXP lonlat, SEXP mcmp) {
	unsigned int i, j, ncol, nrow, ll, next, cmp;
	double **x, zerodist2;
	SEXP ret = NULL;

	ncol = INTEGER_POINTER(pncol)[0];
	ll = INTEGER_POINTER(lonlat)[0];
	cmp = INTEGER_POINTER(mcmp)[0];
	if (ll && ncol != 2)
		error("for longlat data, coordinates should be two-dimensional");
	nrow = LENGTH(pp)/ncol;
	zerodist2 = NUMERIC_POINTER(zero)[0] * NUMERIC_POINTER(zero)[0];
	x = (double **) malloc((size_t) nrow * sizeof(double *));
	if (x == NULL)
		error("could not allocate vector of %u bytes in zerodist",
			nrow * sizeof(double *));
	for (i = 0; i < nrow; i++)
		x[i] = &(NUMERIC_POINTER(pp)[i*ncol]);

	PROTECT(ret = NEW_INTEGER(nrow));
	if (nrow > 0)
		INTEGER_POINTER(ret)[0] = 0;
	for (i = 1; i < nrow; i++) {
		INTEGER_POINTER(ret)[i] = i;
		next = 0;
		for (j = 0; next == 0 && j < i; j++) { /* find match */
			if (INTEGER_POINTER(ret)[j] == j) { /* this is a new point */
				if (is_zero(x[i], x[j], ncol, ll, zerodist2, cmp)) { 
					/* match: */
					INTEGER_POINTER(ret)[i] = j;
					next = 1; /* break for loop */
				}
			}
		} /* for j */
		R_CheckUserInterrupt();
	}
	free(x);
	UNPROTECT(1);
	return(ret);
}
