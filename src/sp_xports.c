#define USING_R 1

#include "sp.h"
/* remember to touch local_stubs.c */

/* SEXP sp_linkingTo_version(void) {
    SEXP ans;
    PROTECT(ans = NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0,
           Rf_mkChar(SP_VERSION));
    UNPROTECT(1);
    return(ans);
} */

SEXP Polygon_c(const SEXP coords, const SEXP n, const SEXP ihole) {

    SEXP SPans, labpt, Area, ringDir, hole, cls;
    double area, xc, yc;
    double *x, *y;
    int pc=0, rev=FALSE;
    int i, ii, nn=INTEGER_POINTER(n)[0];
    SEXP valid;
    SEXP ccopy,  /* copy of coords to go into returned structure */
        dim1;

    for (i=0; i<nn; i++) {
       if(!R_FINITE(NUMERIC_POINTER(coords)[i]))
           Rf_error("non-finite x coordinate");
       if(!R_FINITE(NUMERIC_POINTER(coords)[i+nn]))
           Rf_error("non-finite y coordinate");
    }

// 140406 ring closure
    if (NUMERIC_POINTER(coords)[0] != NUMERIC_POINTER(coords)[nn-1]
        || NUMERIC_POINTER(coords)[nn] != NUMERIC_POINTER(coords)[(2*nn)-1]) {
        PROTECT(ccopy = NEW_NUMERIC((nn+1)*2)); pc++;
        PROTECT(dim1 = NEW_INTEGER(2)); pc++;
        for (i=0; i<nn; i++) {
            NUMERIC_POINTER(ccopy)[i] = NUMERIC_POINTER(coords)[i];
            NUMERIC_POINTER(ccopy)[i+(nn+1)] = NUMERIC_POINTER(coords)[i+nn];
        }
        NUMERIC_POINTER(ccopy)[nn] = NUMERIC_POINTER(coords)[0];
        NUMERIC_POINTER(ccopy)[(2*(nn+1))-1] = NUMERIC_POINTER(coords)[nn];
        nn++;
        INTEGER_POINTER(dim1)[0] = nn;
        INTEGER_POINTER(dim1)[1] = 2;
        Rf_setAttrib(ccopy, R_DimSymbol, dim1);
    } else {
		if (MAYBE_REFERENCED(coords)) {
			PROTECT(ccopy = Rf_duplicate(coords)); 
			pc++;
		} else 
			ccopy = coords; 
	}

	/* from here one, ccopy is closed, and can be modified */

    /* spRFindCG_c(n, ccopy, &xc, &yc, &area); */
    spRFindCG_c(Rf_getAttrib(ccopy, R_DimSymbol), ccopy, &xc, &yc, &area);
    if (fabs(area) < DBL_EPSILON) {
        if (!R_FINITE(xc) || !R_FINITE(yc)) {
            if (nn == 1) {
                xc = NUMERIC_POINTER(ccopy)[0];
                yc = NUMERIC_POINTER(ccopy)[1];
            } else if (nn == 2) {
              xc = (NUMERIC_POINTER(ccopy)[0]+NUMERIC_POINTER(ccopy)[1])/2.0;
              yc = (NUMERIC_POINTER(ccopy)[2]+NUMERIC_POINTER(ccopy)[3])/2.0;
            } else if (nn > 2) {
              xc = (NUMERIC_POINTER(ccopy)[0] +
                NUMERIC_POINTER(ccopy)[(nn-1)])/2.0;
              yc = (NUMERIC_POINTER(ccopy)[nn] +
                NUMERIC_POINTER(ccopy)[nn+(nn-1)])/2.0;
            }
        }
    }

// rchk MAKE_CLASS allocates RSB 180602
    PROTECT(cls = MAKE_CLASS("Polygon")); pc++;
    PROTECT(SPans = NEW_OBJECT(cls)); pc++; 
    PROTECT(ringDir = NEW_INTEGER(1)); pc++;
    INTEGER_POINTER(ringDir)[0] = (area > 0.0) ? -1 : 1;
// -1 cw hole, 1 ccw not-hole

/* RSB 100126 fixing hole assumption
 thanks to Javier Munoz for report */

    if (INTEGER_POINTER(ihole)[0] == NA_INTEGER) { // trust ring direction
        if (INTEGER_POINTER(ringDir)[0] == 1)
            INTEGER_POINTER(ihole)[0] = 0;
        else if (INTEGER_POINTER(ringDir)[0] == -1)
            INTEGER_POINTER(ihole)[0] = 1;
    } else { // trust hole
        if (INTEGER_POINTER(ihole)[0] == 1 && 
            INTEGER_POINTER(ringDir)[0] == 1) {
            rev = TRUE;
            INTEGER_POINTER(ringDir)[0] = -1;
        }
        if (INTEGER_POINTER(ihole)[0] == 0 && 
            INTEGER_POINTER(ringDir)[0] == -1) {
            rev = TRUE;
            INTEGER_POINTER(ringDir)[0] = 1;
        }
    }
    PROTECT(hole = NEW_LOGICAL(1)); pc++;
    if (INTEGER_POINTER(ihole)[0] == 1) LOGICAL_POINTER(hole)[0] = TRUE;
    else LOGICAL_POINTER(hole)[0] = FALSE;

    if (rev) {
        x = (double *) R_alloc((size_t) nn, sizeof(double));
        y = (double *) R_alloc((size_t) nn, sizeof(double));
       for (i=0; i<nn; i++) {
           x[i] = NUMERIC_POINTER(ccopy)[i];
           y[i] = NUMERIC_POINTER(ccopy)[i+nn];
       }
       for (i=0; i<nn; i++) {
           ii = (nn-1)-i;
           NUMERIC_POINTER(ccopy)[ii] = x[i];
           NUMERIC_POINTER(ccopy)[ii+nn] = y[i];
       }
    }

    SET_SLOT(SPans, Rf_install("coords"), ccopy);

    PROTECT(labpt = NEW_NUMERIC(2)); pc++;
    NUMERIC_POINTER(labpt)[0] = xc;
    NUMERIC_POINTER(labpt)[1] = yc;
    SET_SLOT(SPans, Rf_install("labpt"), labpt);

    PROTECT(Area = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(Area)[0] = fabs(area);
    SET_SLOT(SPans, Rf_install("area"), Area);

    SET_SLOT(SPans, Rf_install("hole"), hole);
    SET_SLOT(SPans, Rf_install("ringDir"), ringDir);

    PROTECT(valid = Polygon_validate_c(SPans)); pc++;

    if (! Rf_isLogical(valid)) {
        UNPROTECT(pc);
		/*
        if (isString(valid)) 
			Rf_error(CHAR(STRING_ELT(valid, 0)));
        else 
		*/
			Rf_error("invalid Polygon object");
    }

    UNPROTECT(pc);
    return(SPans);
}

SEXP Polygon_validate_c(const SEXP obj) {

    int pc=0;
    int n;
    SEXP coords, labpt, ans;

    coords = GET_SLOT(obj, Rf_install("coords"));
    n = INTEGER_POINTER(Rf_getAttrib(coords, R_DimSymbol))[0];
    if (NUMERIC_POINTER(coords)[0] != NUMERIC_POINTER(coords)[n-1]
        || NUMERIC_POINTER(coords)[n] != NUMERIC_POINTER(coords)[(2*n)-1]) {
       PROTECT(ans = NEW_CHARACTER(1)); pc++;
       SET_STRING_ELT(ans, 0,
           Rf_mkChar("ring not closed"));
       UNPROTECT(pc);
       return(ans);
    }
    labpt = GET_SLOT(obj, Rf_install("labpt"));
    if (!R_FINITE(NUMERIC_POINTER(labpt)[0]) ||
        !R_FINITE(NUMERIC_POINTER(labpt)[1])) {
        PROTECT(ans = NEW_CHARACTER(1)); pc++;
        SET_STRING_ELT(ans, 0,
           Rf_mkChar("infinite label point"));
       UNPROTECT(pc);
       return(ans);
    }
    PROTECT(ans = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(ans)[0] = TRUE;
    UNPROTECT(pc);
    return(ans);
}

SEXP Polygons_c(const SEXP pls, const SEXP ID) {

    SEXP ans, labpt, Area, plotOrder, crds, pl, n, hole, pls1, ID1, cls;
    int nps, i, pc=0, sumholes;
    double *areas, *areaseps, fuzz;
    int *po, *holes;
    SEXP valid;

	if (MAYBE_REFERENCED(pls)) {
		PROTECT(pls1 = Rf_duplicate(pls)); 
		pc++;
	} else 
		pls1 = pls; 
    // PROTECT(pls1 = MAYBE_REFERENCED(pls) ? Rf_duplicate(pls) : pls); pc++;
	if (MAYBE_REFERENCED(ID)) {
		PROTECT(ID1 = Rf_duplicate(ID)); 
		pc++;
	} else 
		ID1 = ID; 
    // PROTECT(ID1 = MAYBE_REFERENCED(ID) ? Rf_duplicate(ID) : ID); pc++;

    nps = Rf_length(pls1);
    fuzz = R_pow(DBL_EPSILON, (2.0/3.0));
    areas = (double *) R_alloc((size_t) nps, sizeof(double));
    areaseps = (double *) R_alloc((size_t) nps, sizeof(double));
    holes = (int *) R_alloc((size_t) nps, sizeof(int));

    for (i=0, sumholes=0; i<nps; i++) {
        areas[i] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls1, i),
            Rf_install("area")))[0]; 
        holes[i] = LOGICAL_POINTER(GET_SLOT(VECTOR_ELT(pls1, i),
            Rf_install("hole")))[0];
         areaseps[i] = holes[i] ? areas[i] + fuzz : areas[i];
         sumholes += holes[i];
    }
    po = (int *) R_alloc((size_t) nps, sizeof(int));
    if (nps > 1) {
        for (i=0; i<nps; i++)
			po[i] = i + R_OFFSET;
        revsort(areaseps, po, nps);
    } else
        po[0] = 1;

    if (sumholes == nps) {
        crds = GET_SLOT(VECTOR_ELT(pls1, (po[0] - R_OFFSET)),
            Rf_install("coords"));
        PROTECT(n = NEW_INTEGER(1)); pc++;
        INTEGER_POINTER(n)[0] = INTEGER_POINTER(Rf_getAttrib(crds,
            R_DimSymbol))[0];
        PROTECT(hole = NEW_LOGICAL(1)); pc++;
        LOGICAL_POINTER(hole)[0] = FALSE;
        pl = Polygon_c(crds, n, hole);
/* bug 100417 Patrick Giraudoux */
        holes[po[0] - R_OFFSET] = LOGICAL_POINTER(hole)[0];
        SET_VECTOR_ELT(pls1, (po[0] - R_OFFSET), pl);
    }

// rchk MAKE_CLASS allocates RSB 180602
    PROTECT(cls = MAKE_CLASS("Polygons")); pc++;
    PROTECT(ans = NEW_OBJECT(cls)); pc++;
    SET_SLOT(ans, Rf_install("Polygons"), pls1);
    SET_SLOT(ans, Rf_install("ID"), ID1);

    PROTECT(Area = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(Area)[0] = 0.0;
    for (i=0; i<nps; i++)
        NUMERIC_POINTER(Area)[0] += holes[i] ? 0.0 : fabs(areas[i]);

    SET_SLOT(ans, Rf_install("area"), Area);

    PROTECT(plotOrder = NEW_INTEGER(nps)); pc++;
    for (i=0; i<nps; i++)
		INTEGER_POINTER(plotOrder)[i] = po[i];

    SET_SLOT(ans, Rf_install("plotOrder"), plotOrder);

    PROTECT(labpt = NEW_NUMERIC(2)); pc++;
    NUMERIC_POINTER(labpt)[0] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls1,
        (po[0]-1)), Rf_install("labpt")))[0];
    NUMERIC_POINTER(labpt)[1] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls1,
        (po[0]-1)), Rf_install("labpt")))[1];
    SET_SLOT(ans, Rf_install("labpt"), labpt);

    PROTECT(valid = Polygons_validate_c(ans)); pc++;
    if (!Rf_isLogical(valid)) {
        UNPROTECT(pc);
		/*
        if (isString(valid)) 
			Rf_error(CHAR(STRING_ELT(valid, 0)));
        else
		*/
			Rf_error("invalid Polygons object");
    }

    UNPROTECT(pc);
    return(ans);
}

SEXP Polygons_validate_c(const SEXP obj) {

    int pc=0;
    int i, n;
    SEXP Pls, labpt, ans;
    char *cls="Polygon";

    PROTECT(Pls = GET_SLOT(obj, Rf_install("Polygons"))); pc++;
    n = Rf_length(Pls);
    for (i=0; i<n; i++) {
        if (strcmp(CHAR(STRING_ELT(Rf_getAttrib(VECTOR_ELT(Pls, i),
           R_ClassSymbol), 0)), cls) != 0) {
              PROTECT(ans = NEW_CHARACTER(1)); pc++;
              SET_STRING_ELT(ans, 0,
              Rf_mkChar("Polygons slot contains non-Polygon object"));
              UNPROTECT(pc);
              return(ans);
        }
    }

    if (n != Rf_length(GET_SLOT(obj, Rf_install("plotOrder")))) {
        PROTECT(ans = NEW_CHARACTER(1)); pc++;
        SET_STRING_ELT(ans, 0,
           Rf_mkChar("plotOrder and Polygons differ in length"));
        UNPROTECT(pc);
        return(ans);
    }

    labpt = GET_SLOT(obj, Rf_install("labpt"));
    if (!R_FINITE(NUMERIC_POINTER(labpt)[0]) ||
      !R_FINITE(NUMERIC_POINTER(labpt)[1])) {
      PROTECT(ans = NEW_CHARACTER(1)); pc++;
      SET_STRING_ELT(ans, 0,
         Rf_mkChar("infinite label point"));
      UNPROTECT(pc);
      return(ans);
    }
    PROTECT(ans = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(ans)[0] = TRUE;
    UNPROTECT(pc);
    return(ans);

}

SEXP SpatialPolygons_c(const SEXP pls, const SEXP pO, 
		const SEXP p4s) {

    SEXP ans, bbox, ppO, cls;
    int pc=0;

// rchk MAKE_CLASS allocates RSB 180602
    PROTECT(cls = MAKE_CLASS("SpatialPolygons")); pc++;
    PROTECT(ans = NEW_OBJECT(cls)); pc++;
    // SET_SLOT(ans, Rf_install("polygons"), MAYBE_REFERENCED(pls) ? Rf_duplicate(pls) : pls);
    SET_SLOT(ans, Rf_install("polygons"), pls);
    // SET_SLOT(ans, Rf_install("proj4string"), MAYBE_REFERENCED(p4s) ? Rf_duplicate(p4s) : p4s);
    SET_SLOT(ans, Rf_install("proj4string"), p4s);

    if (pO == R_NilValue) {
		PROTECT(ppO = SpatialPolygons_plotOrder_c(pls));
		pc++;
    } else
		ppO = pO;
    // SET_SLOT(ans, Rf_install("plotOrder"), MAYBE_REFERENCED(pO) ? Rf_duplicate(pO) : pO);
    SET_SLOT(ans, Rf_install("plotOrder"), ppO);

    PROTECT(bbox = bboxCalcR_c(pls)); pc++;
    SET_SLOT(ans, Rf_install("bbox"), bbox);

    UNPROTECT(pc);
    return(ans);

}

SEXP SpatialPolygons_plotOrder_c(const SEXP pls) {

    SEXP plotOrder, pls1;
    int pc=0, ng, i;
    int *po;
    double *areas;

    // PROTECT(pls1 = MAYBE_REFERENCED(pls) ? Rf_duplicate(pls) : pls); pc++;
	if (MAYBE_REFERENCED(pls)) {
		PROTECT(pls1 = Rf_duplicate(pls));
		pc++;
	} else
		pls1 = pls;

    ng = Rf_length(pls1);
    areas = (double *) R_alloc((size_t) ng, sizeof(double));
    po = (int *) R_alloc((size_t) ng, sizeof(int));
    for (i=0; i<ng; i++) {
        areas[i] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls1, i),
            Rf_install("area")))[0]; 
        po[i] = i + R_OFFSET;
    }
    revsort(areas, po, ng);
    PROTECT(plotOrder = NEW_INTEGER(ng)); pc++;
    for (i=0; i<ng; i++)
		INTEGER_POINTER(plotOrder)[i] = po[i];

    UNPROTECT(pc);
    return(plotOrder);

}

SEXP SpatialPolygons_validate_c(const SEXP obj) {

    int pc=0;
    int i, n;
    SEXP pls, ans;
    char *cls="Polygons";

    PROTECT(pls = GET_SLOT(obj, Rf_install("polygons"))); pc++;
    n = Rf_length(pls);
    for (i=0; i<n; i++) {
        if (strcmp(CHAR(STRING_ELT(Rf_getAttrib(VECTOR_ELT(pls, i),
           R_ClassSymbol), 0)), cls) != 0) {
             PROTECT(ans = NEW_CHARACTER(1)); pc++;
             SET_STRING_ELT(ans, 0,
             Rf_mkChar("polygons slot contains non-Polygons object"));
             UNPROTECT(pc);
             return(ans);
        }
    }

    if (n != Rf_length(GET_SLOT(obj, Rf_install("plotOrder")))) {
        PROTECT(ans = NEW_CHARACTER(1)); pc++;
        SET_STRING_ELT(ans, 0,
           Rf_mkChar("plotOrder and polygons differ in length"));
        UNPROTECT(pc);
        return(ans);
    }

    PROTECT(ans = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(ans)[0] = TRUE;
    UNPROTECT(pc);
    return(ans);

}

SEXP SpatialPolygons_getIDs_c(const SEXP obj) {

    int pc=0;
    int i, n;
    SEXP pls, IDs, obj1;

    // PROTECT(obj1 = MAYBE_REFERENCED(obj) ? Rf_duplicate(obj) : obj); pc++;
	if (MAYBE_REFERENCED(obj)) {
		PROTECT(obj1 = Rf_duplicate(obj));
		pc++;
	} else
		obj1 = obj;

    PROTECT(pls = GET_SLOT(obj, Rf_install("polygons"))); pc++;
    n = Rf_length(pls);
    PROTECT(IDs = NEW_CHARACTER(n)); pc++;
    for (i=0; i<n; i++)
        SET_STRING_ELT(IDs, i, STRING_ELT(GET_SLOT(VECTOR_ELT(pls, i),
            Rf_install("ID")), 0));
      
    UNPROTECT(pc);
    return(IDs);

}

SEXP bboxCalcR_c(const SEXP pls) {

    SEXP ans, dim, dimnames, Pl, crds, pls1;
    double UX=-DBL_MAX, LX=DBL_MAX, UY=-DBL_MAX, LY=DBL_MAX;
    int i, j, k, n, npls, npl, pc=0;
    double x, y;

    // PROTECT(pls1 = MAYBE_REFERENCED(pls) ? Rf_duplicate(pls) : pls); pc++;
	if (MAYBE_REFERENCED(pls)) {
		PROTECT(pls1 = Rf_duplicate(pls));
		pc++;
	} else
		pls1 = pls;

    npls = Rf_length(pls1);
	if (npls == 0) { /* EJP: this makes a zero-length object at least pass further sanity checks */
		UX = UY = DBL_MAX;
		LX = LY = -DBL_MAX;
	} else for (i=0; i<npls; i++) {
        Pl = GET_SLOT(VECTOR_ELT(pls1, i), Rf_install("Polygons"));
        npl = Rf_length(Pl);
        for (j=0; j<npl; j++) {
            crds = GET_SLOT(VECTOR_ELT(Pl, j), Rf_install("coords"));
            n = INTEGER_POINTER(Rf_getAttrib(crds, R_DimSymbol))[0];
            for (k=0; k<n; k++) {
               x = NUMERIC_POINTER(crds)[k];
               y = NUMERIC_POINTER(crds)[k+n];
               if (x > UX) UX = x;
               if (y > UY) UY = y;
               if (x < LX) LX = x;
               if (y < LY) LY = y;
            }
        }
    }

    PROTECT(ans = NEW_NUMERIC(4)); pc++;
    NUMERIC_POINTER(ans)[0] = LX;
    NUMERIC_POINTER(ans)[1] = LY;
    NUMERIC_POINTER(ans)[2] = UX;
    NUMERIC_POINTER(ans)[3] = UY;
    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = 2;
    INTEGER_POINTER(dim)[1] = 2;
    Rf_setAttrib(ans, R_DimSymbol, dim);
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 0, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 0, Rf_mkChar("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 1, Rf_mkChar("y"));
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, Rf_mkChar("min"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, Rf_mkChar("max"));
    Rf_setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(pc);
    return(ans);

}

void spRFindCG_c(const SEXP n, const SEXP coords, 
		double *xc, double *yc, double *area ) {

	int i, nn;
	tPointd *P;
	tPointd CG;
	double Areasum2;

	nn = INTEGER_POINTER(n)[0];
	P = (tPointd *) R_alloc((size_t) nn, sizeof(tPointd));
	for (i=0; i<nn; i++) {
		P[i][0] = NUMERIC_POINTER(coords)[i];
		P[i][1] = NUMERIC_POINTER(coords)[i+nn];
	}
	FindCG(nn, P, CG, &Areasum2);
	xc[0] = CG[0];
	yc[0] = CG[1];
	area[0] = Areasum2/2;
	return;
}

void FindCG( int n, tPointd *P, tPointd CG, double *Areasum2) {
	int     i;
	double  A2;        /* Partial area sum */    
	tPointd Cent3;

	CG[0] = 0;
	CG[1] = 0;
	Areasum2[0] = 0;
	for (i = 1; i < n-1; i++) {
		Centroid3( P[0], P[i], P[i+1], Cent3 );
		A2 =  Area2( P[0], P[i], P[i+1]);
		CG[0] += A2 * Cent3[0];
		CG[1] += A2 * Cent3[1];
		Areasum2[0] += A2;
	}
	CG[0] /= 3 * Areasum2[0];
	CG[1] /= 3 * Areasum2[0];
	return;
}
/*
	Returns three times the centroid.  The factor of 3 is
	left in to permit division to be avoided until later.
*/
void Centroid3(const tPointd p1, const tPointd p2, const tPointd p3, 
	tPointd c) {
        c[0] = p1[0] + p2[0] + p3[0];
        c[1] = p1[1] + p2[1] + p3[1];
	return;
}
/* 
        Returns twice the signed area of the triangle determined by a,b,c,
        positive if a,b,c are oriented ccw, and negative if cw.
*/
double Area2(const tPointd a, const tPointd b, const tPointd c) {
	double area;
	area = (b[0] - a[0]) * (c[1] - a[1]) - (c[0] - a[0]) * (b[1] - a[1]);
	return(area);
}


SEXP comment2comm(const SEXP obj) {
    SEXP ans, comment, obj1;
    int pc=0, ns, i, j, jj, k, nc;
    char s[15], *buf;
    int *c, *nss, *co, *coo;

    // PROTECT(obj1 = MAYBE_REFERENCED(obj) ? Rf_duplicate(obj) : obj); pc++;
	if (MAYBE_REFERENCED(obj)) {
		PROTECT(obj1 = Rf_duplicate(obj));
		pc++;
	} else
		obj1 = obj;

    PROTECT(comment = Rf_getAttrib(obj1, Rf_install("comment"))); pc++;
    if (comment == R_NilValue) {
        UNPROTECT(pc);
        return(R_NilValue);
    }

    nc = Rf_length(STRING_ELT(comment, 0));
    if (nc < 1) Rf_error("comment2comm: empty string comment");

    buf = (char *) R_alloc((size_t) (nc+1), sizeof(char));

    strcpy(buf, CHAR(STRING_ELT(comment, 0)));

    i = 0;
    ns = 0;
    while (buf[i] != '\0') {
        if (buf[i] == ' ') ns++;
        i++;
    }
    k = (int) strlen(buf);

   
    nss = (int *) R_alloc((size_t) (ns+1), sizeof(int));
    c = (int *) R_alloc((size_t) (ns+1), sizeof(int));
    i = 0;
    j = 0;
    while (buf[i] != '\0') {
        if (buf[i] == ' ') {
            nss[j] = i; j++;
        }
        i++;
    }
    nss[(ns)] = k;

    s[0] = '\0';
    if (nss[0] > 15) Rf_error("comment2comm: buffer overflow");
    strncpy(s, &buf[0], (size_t) nss[0]);
    s[nss[0]] = '\0';

    c[0] = atoi(s);
    for (i=0; i<ns; i++) {
        k = nss[(i+1)]-(nss[i]+1);
        if (k > 15) Rf_error("comment2comm: buffer overflow");
        strncpy(s, &buf[(nss[i]+1)], (size_t) k);
        s[k] = '\0';
        c[i+1] = atoi(s);
    }

    for (i=0, k=0; i<(ns+1); i++) if (c[i] == 0) k++;
    
    PROTECT(ans = NEW_LIST((k))); pc++;
    co = (int *) R_alloc((size_t) k, sizeof(int));
    coo = (int *) R_alloc((size_t) k, sizeof(int));
    for (i=0; i<k; i++) co[i] = 1;

    for (i=0, j=0; i<(ns+1); i++)
        if (c[i] == 0) coo[j++] = i + R_OFFSET;

    for (i=0; i<k; i++)
        for (j=0; j<(ns+1); j++)
            if ((c[j]) == coo[i]) co[i]++;

    for (i=0; i<k; i++) SET_VECTOR_ELT(ans, i, NEW_INTEGER(co[i]));

    for (i=0; i<k; i++) {
        jj = 0;
        INTEGER_POINTER(VECTOR_ELT(ans, i))[jj++] = coo[i];
        if (co[i] > 1) {
            for (j=0; j<(ns+1); j++)
                if (c[j] == coo[i])
                    INTEGER_POINTER(VECTOR_ELT(ans, i))[jj++] = j + R_OFFSET;
        }
    }

    UNPROTECT(pc);
    return(ans);
}

void comm2comment(char *buf, int bufsiz, int *comm, int nps) {
    char cbuf[15];
    int i, nc, nc1, pr;

    nc = (int) (ceil(log10(nps)+1.0)+1.0);
    nc1 = (nc*nps)+1;
    if (bufsiz < nc1) Rf_error("comm2comment: buffer overflow");

    pr = snprintf(buf, bufsiz, "%d", comm[0]);
    bufsiz -= pr;
    for (i = 1; i < nps; i++) {
        snprintf(cbuf, 15, " %d", comm[i]);
        if (strlen(cbuf) >= bufsiz)
            Rf_error("comm2comment: buffer overflow");
        strncat(buf, cbuf, bufsiz);
        bufsiz -= strlen(cbuf);
    }
    strcat(buf, "\0");
    return;
}


