#include "collapse_c.h"

// Faster version of base R's spit based on grouping objects..
// Support DF's?
// -> works for factors, Date and POSIXct, but not for POSIXlt (handeled in R)
// TODO: SIMD / multithreading? -> I checked SIMD doesn't work, and multithreading hardly give any performance gains.
// The largest cost anyways is lapply(), not gsplit() !!

SEXP gsplit(SEXP x, SEXP gobj, SEXP toint) {
  if(TYPEOF(gobj) != VECSXP || !inherits(gobj, "GRP")) error("g needs to be an object of class 'GRP', see ?GRP");
  const SEXP g = VECTOR_ELT(gobj, 1), gs = VECTOR_ELT(gobj, 2),
    ord = VECTOR_ELT(gobj, 5), order = VECTOR_ELT(gobj, 6);
  const int ng = length(gs), *pgs = INTEGER(gs), tx = TYPEOF(x), l = length(g);
  if(ng != INTEGER(VECTOR_ELT(gobj, 0))[0]) error("'GRP' object needs to have valid vector of group-sizes");
  SEXP res = PROTECT(allocVector(VECSXP, ng));
  // Output as integer or not
  if(asLogical(toint)) {
    for(int i = 0; i != ng; ++i) SET_VECTOR_ELT(res, i, allocVector(INTSXP, pgs[i]));
  } else { // Allocate split vectors and copy attributes and object bits
    SEXP x1 = PROTECT(allocVector(tx, 1));
    copyMostAttrib(x, x1);
    SEXP ax = ATTRIB(x1);
    if(length(ax) == 1 && TAG(ax) == install("label")) ax = R_NilValue;
    int ox = OBJECT(x);
    // FAZIT: Need to use SET_VECTOR_ELT!! pres[i] = allocVector() doesn't work!!
    if(TYPEOF(ax) != NILSXP && ox != 0) {
      for(int i = 0, s4o = IS_S4_OBJECT(x); i != ng; ++i) {
        SEXP resi;
        SET_VECTOR_ELT(res, i, resi = allocVector(tx, pgs[i]));
        SET_ATTRIB(resi, ax);
        SET_OBJECT(resi, ox);
        if(s4o) SET_S4_OBJECT(resi);
      }
    } else if(TYPEOF(ax) != NILSXP) {
      for(int i = 0; i != ng; ++i) {
        SEXP resi;
        SET_VECTOR_ELT(res, i, resi = allocVector(tx, pgs[i])); // SET_ATTRIB(pres[i] = allocVector(tx, pgs[i]), ax);
        SET_ATTRIB(resi, ax);
      }
    } else if(ox != 0) { // Is this even possible? Object bits but no attributes?
      for(int i = 0, s4o = IS_S4_OBJECT(x); i != ng; ++i) {
        SEXP resi;
        SET_VECTOR_ELT(res, i, resi = allocVector(tx, pgs[i]));
        SET_OBJECT(resi, ox);
        if(s4o) SET_S4_OBJECT(resi);
      }
    } else {
      for(int i = 0; i != ng; ++i) SET_VECTOR_ELT(res, i, allocVector(tx, pgs[i]));
    }
    UNPROTECT(1);
  }

  const SEXP *restrict pres = SEXPPTR_RO(res);
  // If grouping is sorted
  if(LOGICAL(ord)[1] == 1) { // This only works if data is already ordered in order of the groups
    int count = 0;
    if(asLogical(toint)) {
      for(int j = 0; j != ng; ++j) {
        int *pgj = INTEGER(pres[j]), gsj = pgs[j];
        for(int i = 0; i != gsj; ++i) pgj[i] = ++count;
      }
    } else {
      if(length(x) != l) error("length(x) must match length(g)");
      switch(tx) {
      case INTSXP:
      case LGLSXP: {
        const int *px = INTEGER(x);
        for(int j = 0; j != ng; ++j) {
          int *pgj = INTEGER(pres[j]), gsj = pgs[j];
          for(int i = 0; i != gsj; ++i) pgj[i] = px[count++];
        }
        break;
      }
      case REALSXP: {
        const double *px = REAL(x);
        for(int j = 0, gsj; j != ng; ++j) {
          double *pgj = REAL(pres[j]);
          gsj = pgs[j];
          for(int i = 0; i != gsj; ++i) pgj[i] = px[count++];
        }
        break;
      }
      case CPLXSXP: {
        const Rcomplex *px = COMPLEX(x);
        for(int j = 0, gsj; j != ng; ++j) {
          Rcomplex *pgj = COMPLEX(pres[j]);
          gsj = pgs[j];
          for(int i = 0; i != gsj; ++i) pgj[i] = px[count++];
        }
        break;
      }
      case STRSXP: {
        const SEXP *px = STRING_PTR(x);
        for(int j = 0, gsj; j != ng; ++j) {
          SEXP *pgj = STRING_PTR(pres[j]);
          gsj = pgs[j];
          for(int i = 0; i != gsj; ++i) pgj[i] = px[count++];
        }
        break;
      }
      case VECSXP: {
        const SEXP *px = SEXPPTR_RO(x);
        for(int j = 0, gsj; j != ng; ++j) {
          SEXP *pgj = SEXPPTR(pres[j]);
          gsj = pgs[j];
          for(int i = 0; i != gsj; ++i) pgj[i] = px[count++];
        }
        break;
      }
      case RAWSXP: {
        const Rbyte *px = RAW(x);
        for(int j = 0, gsj; j != ng; ++j) {
          Rbyte *pgj = RAW(pres[j]);
          gsj = pgs[j];
          for(int i = 0; i != gsj; ++i) pgj[i] = px[count++];
        }
        break;
      }
      default: error("Unsupported type '%s' passed to gsplit", type2char(tx));
      }
    }
  } else if(length(order) == l) { // Grouping not sorted but we have the ordering..
    SEXP sym_starts = PROTECT(install("starts"));
    const SEXP starts = getAttrib(order, sym_starts);
    UNPROTECT(1);
    if(length(starts) != ng) goto unsno;
    const int *po = INTEGER(order), *ps = INTEGER(starts);

    if(asLogical(toint)) {
      for(int i = 0; i != ng; ++i) {
        int *pri = INTEGER(pres[i]);
        for(int j = ps[i]-1, end = ps[i]+pgs[i]-1, k = 0; j < end; j++) pri[k++] = po[j];
      }
    } else {
      if(length(x) != l) error("length(x) must match length(g)");
      switch(tx) {
      case INTSXP:
      case LGLSXP: {
        const int *px = INTEGER(x);
        for(int i = 0; i != ng; ++i) {
          int *pri = INTEGER(pres[i]);
          for(int j = ps[i]-1, end = ps[i]+pgs[i]-1, k = 0; j < end; ++j) pri[k++] = px[po[j]-1];
        }
        break;
      }
      case REALSXP: {
        double *px = REAL(x);
        for(int i = 0; i != ng; ++i) {
          double *pri = REAL(pres[i]);
          for(int j = ps[i]-1, end = ps[i]+pgs[i]-1, k = 0; j < end; ++j) pri[k++] = px[po[j]-1];
        }
        break;
      }
      case CPLXSXP: {
        Rcomplex *px = COMPLEX(x);
        for(int i = 0; i != ng; ++i) {
          Rcomplex *pri = COMPLEX(pres[i]);
          for(int j = ps[i]-1, end = ps[i]+pgs[i]-1, k = 0; j < end; ++j) pri[k++] = px[po[j]-1];
        }
        break;
      }
      case STRSXP: {
        SEXP *px = STRING_PTR(x);
        for(int i = 0; i != ng; ++i) {
          SEXP *pri = STRING_PTR(pres[i]);
          for(int j = ps[i]-1, end = ps[i]+pgs[i]-1, k = 0; j < end; ++j) pri[k++] = px[po[j]-1];
        }
        break;
      }
      case VECSXP: {
        const SEXP *px = SEXPPTR_RO(x);
        for(int i = 0; i != ng; ++i) {
          SEXP *pri = SEXPPTR(pres[i]);
          for(int j = ps[i]-1, end = ps[i]+pgs[i]-1, k = 0; j < end; ++j) pri[k++] = px[po[j]-1];
        }
        break;
      }
      case RAWSXP: {
        const Rbyte *px = RAW(x);
        for(int i = 0; i != ng; ++i) {
          Rbyte *pri = RAW(pres[i]);
          for(int j = ps[i]-1, end = ps[i]+pgs[i]-1, k = 0; j < end; ++j) pri[k++] = px[po[j]-1];
        }
        break;
      }
      default: error("Unsupported type '%s' passed to gsplit", type2char(tx));
      }
    }

  } else { // Unsorted, without ordering
    unsno:;
    int *count = (int*)Calloc(ng, int);
    // memset(count, 0, sizeof(int)*(ng+1)); // Needed here ??
    // int *count = (int *) R_alloc(ng+1, sizeof(int));

    const int *pg = INTEGER(g);
    // --pres;
    if(asLogical(toint)) {
      for(int i = 0, gi; i != l; ++i) {
        gi = pg[i]-1;
        INTEGER(pres[gi])[count[gi]++] = i+1;
      }
    } else {
      if(length(x) != l) error("length(x) must match length(g)");
      switch(tx) {
      case INTSXP:
      case LGLSXP: {
        const int *px = INTEGER(x);
        for(int i = 0, gi; i != l; ++i) {
          gi = pg[i]-1;
          INTEGER(pres[gi])[count[gi]++] = px[i];
        }
        break;
      }
      case REALSXP: {
        const double *px = REAL(x);
        for(int i = 0, gi; i != l; ++i) {
          gi = pg[i]-1;
          REAL(pres[gi])[count[gi]++] = px[i];
        }
        break;
      }
      case CPLXSXP: {
        const Rcomplex *px = COMPLEX(x);
        for(int i = 0, gi; i != l; ++i) {
          gi = pg[i]-1;
          COMPLEX(pres[gi])[count[gi]++] = px[i];
        }
        break;
      }
      case STRSXP: {
        const SEXP *px = STRING_PTR(x);
        for(int i = 0, gi; i != l; ++i) {
          gi = pg[i]-1;
          STRING_PTR(pres[gi])[count[gi]++] = px[i];
        }
        break;
      }
      case VECSXP: {
        const SEXP *px = SEXPPTR_RO(x);
        for(int i = 0, gi; i != l; ++i) {
          gi = pg[i]-1;
          SEXPPTR(pres[gi])[count[gi]++] = px[i];
        }
        break;
      }
      case RAWSXP: {
        const Rbyte *px = RAW(x);
        for(int i = 0, gi; i != l; ++i) {
          gi = pg[i]-1;
          RAW(pres[gi])[count[gi]++] = px[i];
        }
        break;
      }
      default: error("Unsupported type '%s' passed to gsplit", type2char(tx));
      }
    }
    Free(count);
  }
  UNPROTECT(1);
  return res;
}

// This is for fmutate, to reorder the result of grouped data if the result has the same length as x
SEXP greorder(SEXP x, SEXP gobj) {
  if(TYPEOF(gobj) != VECSXP || !inherits(gobj, "GRP")) error("g needs to be an object of class 'GRP', see ?GRP");
  const SEXP g = VECTOR_ELT(gobj, 1), gs = VECTOR_ELT(gobj, 2), order = VECTOR_ELT(gobj, 6);
  const int ng = length(gs), l = length(g), tx = TYPEOF(x),
            *pgs = INTEGER(gs), *pg = INTEGER(g);
  if(l != length(x)) error("length(x) must match length(g)");
  if(ng != INTEGER(VECTOR_ELT(gobj, 0))[0]) error("'GRP' object needs to have valid vector of group-sizes");
  if(LOGICAL(VECTOR_ELT(gobj, 5))[1] == 1) return x;

  SEXP res = PROTECT(allocVector(tx, l));

  // Note: This is only faster for a large number of groups...
  if(length(order) == l) { // Grouping not sorted but we have the ordering..
    SEXP sym_starts = PROTECT(install("starts"));
    const SEXP starts = getAttrib(order, sym_starts);
    UNPROTECT(1);
    if(length(starts) != ng) goto unsno2;
    const int *po = INTEGER(order), *ps = INTEGER(starts);

    switch(tx) {
    case INTSXP:
    case LGLSXP: {
      int *px = INTEGER(x), *pr = INTEGER(res);
      for(int i = 0, k = 0; i != ng; ++i) {
        for(int j = ps[i]-1, end = ps[i]+pgs[i]-1; j < end; ++j) pr[po[j]-1] = px[k++];
      }
      break;
    }
    case REALSXP: {
      double *px = REAL(x), *pr = REAL(res);
      for(int i = 0, k = 0; i != ng; ++i) {
        for(int j = ps[i]-1, end = ps[i]+pgs[i]-1; j < end; ++j) pr[po[j]-1] = px[k++];
      }
      break;
    }
    case CPLXSXP: {
      Rcomplex *px = COMPLEX(x), *pr = COMPLEX(res);
      for(int i = 0, k = 0; i != ng; ++i) {
        for(int j = ps[i]-1, end = ps[i]+pgs[i]-1; j < end; ++j) pr[po[j]-1] = px[k++];
      }
      break;
    }
    case STRSXP: {
      SEXP *px = STRING_PTR(x), *pr = STRING_PTR(res);
      for(int i = 0, k = 0; i != ng; ++i) {
        for(int j = ps[i]-1, end = ps[i]+pgs[i]-1; j < end; ++j) pr[po[j]-1] = px[k++];
      }
      break;
    }
    case VECSXP: {
      SEXP *pr = SEXPPTR(res);
      const SEXP *px = SEXPPTR_RO(x);
      for(int i = 0, k = 0; i != ng; ++i) {
        for(int j = ps[i]-1, end = ps[i]+pgs[i]-1; j < end; ++j) pr[po[j]-1] = px[k++];
      }
      break;
    }
    case RAWSXP: {
      Rbyte *px = RAW(x), *pr = RAW(res);
      for(int i = 0, k = 0; i != ng; ++i) {
        for(int j = ps[i]-1, end = ps[i]+pgs[i]-1; j < end; ++j) pr[po[j]-1] = px[k++];
      }
      break;
    }
    default: error("Unsupported type '%s' passed to gsplit", type2char(tx));
    }

  } else { // Unsorted, without ordering
    unsno2:;
    int *count = (int *) R_alloc(ng+1, sizeof(int));
    int *cgs = (int *) R_alloc(ng+2, sizeof(int)); cgs[1] = 0;
    for(int i = 0; i != ng; ++i) {
      count[i+1] = 0;
      cgs[i+2] = cgs[i+1] + pgs[i];
    }
    switch(tx) {
    case INTSXP:
    case LGLSXP: {
      int *px = INTEGER(x), *pr = INTEGER(res);
      for(int i = 0; i != l; ++i) pr[i] = px[cgs[pg[i]]+count[pg[i]]++];
      break;
    }
    case REALSXP: {
      double *px = REAL(x), *pr = REAL(res);
      for(int i = 0; i != l; ++i) pr[i] = px[cgs[pg[i]]+count[pg[i]]++];
      break;
    }
    case CPLXSXP: {
      Rcomplex *px = COMPLEX(x), *pr = COMPLEX(res);
      for(int i = 0; i != l; ++i) pr[i] = px[cgs[pg[i]]+count[pg[i]]++];
      break;
    }
    case STRSXP: {
      SEXP *px = STRING_PTR(x), *pr = STRING_PTR(res);
      for(int i = 0; i != l; ++i) pr[i] = px[cgs[pg[i]]+count[pg[i]]++];
      break;
    }
    case VECSXP: {
      SEXP *pr = SEXPPTR(res);
      const SEXP *px = SEXPPTR_RO(x);
      for(int i = 0; i != l; ++i) pr[i] = px[cgs[pg[i]]+count[pg[i]]++];
      break;
    }
    case RAWSXP: {
      Rbyte *px = RAW(x), *pr = RAW(res);
      for(int i = 0; i != l; ++i) pr[i] = px[cgs[pg[i]]+count[pg[i]]++];
      break;
    }
    default: error("Unsupported type '%s' passed to gsplit", type2char(tx));
    }
  }
  SHALLOW_DUPLICATE_ATTRIB(res, x);
  UNPROTECT(1);
  return res;
}
