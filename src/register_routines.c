#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
// #include <R_ext/RS.h>


/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}


/* .Fortran calls */
extern void F77_NAME(droughtIndicator)( SEXP SMI, SEXP mask, SEXP SMI_thld, SEXP cellCoor , SEXP SMIc);
extern void F77_NAME(ClusterEvolution)( SEXP SMIc, int nrows, int ncols, int nMonths, 
    int nCells, SEXP cellCoor, SEXP nCellInter, SEXP thCellClus);
extern void F77_NAME(ClusterStats)( SEXP SMI, SEXP mask, int nrows, int ncols, 
    int nMonths, int nCells, double SMI_thld );
extern void F77_NAME(calSAD)(SEXP SMI, SEXP mask, SEXP iDur, int nrows, int ncols, 
    int nMonths, int nCells, SEXP deltaArea, double cellsize);
extern void F77_NAME(findClusters)(SEXP cellCoor, SEXP thCellClus, SEXP t, SEXP iC,
    int nCluster, int nrows, int ncols, int nCells, SEXP SMIc);

static const R_FortranMethodDef FortranEntries[] = {
    CALLDEF(droughtIndicator, 5), 
    CALLDEF(ClusterEvolution, 8), 
    CALLDEF(ClusterStats, 7), 
    CALLDEF(calSAD, 9), 
    CALLDEF(findClusters, 9), 
    {NULL, NULL, 0}
};

void R_init_SMI(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
