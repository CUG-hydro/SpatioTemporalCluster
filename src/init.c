#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(clusterevolution)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(clusterstats2)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(droughtindicator)(void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"clusterevolution", (DL_FUNC) &F77_NAME(clusterevolution), 11},
    {"clusterstats2",    (DL_FUNC) &F77_NAME(clusterstats2),    13},
    {"droughtindicator", (DL_FUNC) &F77_NAME(droughtindicator),  8},
    {NULL, NULL, 0}
};

void R_init_SMI(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
// tools::package_native_routine_registration_skeleton(".")
