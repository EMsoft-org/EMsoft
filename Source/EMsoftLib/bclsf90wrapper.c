// =====================================================================
// $Revision: 290 $ $Date: 2007-03-04 22:01:54 -0800 (Sun, 04 Mar 2007) $
//
// Driver for BCLS: a bound-constrained least-squares solver.
//
// 25 Aug 05: Original version.
//            Michael P. Friedlander
//            mpf@cs.ubc.ca
//            University of British Columbia
// 15 May 17: modified original bcsol.c program to be 
//            compatible with EMsoft library. Aim is to
//   	      solve the pole figure inversion problem as
//            a tomographic reconstruction problem
//	      Saransh Singh
//            ssaransh@andrew.cmu.edu
//	      Carnegie Mellon University
//
// =====================================================================

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "bcls/bcls.h"  // The BCLS solver library.

#include "cs.h"    // The CSparse matrix library.


// ---------------------------------------------------------------------
// Workspace structure passed to Aprod and Usolve routines.
// ---------------------------------------------------------------------
typedef struct {
    cs *A;
} worksp;

// ---------------------------------------------------------------------
// xmalloc
// Malloc wrapper.
//
// Returns a pointer to the newly allocated memory, or NULL if malloc
// returned an eror.
// ---------------------------------------------------------------------
static void *
xmalloc(int n, size_t size, char * who) {

    register void *value = malloc(n * size);
    if (value == NULL) {
	fprintf( stderr, "Not enough memory to allocate %s.\n", who );
	exit(EXIT_FAILURE);
    }
    return value;
}

// ---------------------------------------------------------------------
// dload
// Load a constant into a vector.
// ---------------------------------------------------------------------
static void
dload( const int n, const double alpha, double x[] ) {

    int j;
    for (j = 0; j < n; j++) x[j] = alpha;
    return;
}

// ---------------------------------------------------------------------
// Aprod
// Matrix-vector products.
//
// If     mode == BCLS_PROD_A  (0), compute y <- A *x, with x untouched;
// and if mode == BCLS_PROD_At (1), compute x <- A'*y, with y untouched.
// ---------------------------------------------------------------------
static int
Aprod( int mode, int m, int n, int nix, int ix[],
       double x[], double y[], void *UsrWrk ) {

    int     i, j, k, l;
    double  aij, xj, sum;
    worksp * Wrk = (worksp *)UsrWrk;
    cs *A = (cs *)Wrk->A;

    assert( mode == BCLS_PROD_A     ||
            mode == BCLS_PROD_At    ||
            mode == BCLS_PROD_INIT  ||
            mode == BCLS_PROD_TERM  );
    assert( nix  <= n );
    assert( A->m == m );
    assert( A->n == n );

    if (mode == BCLS_PROD_A) {

        dload( m, 0.0, y );

	for (l = 0; l < nix; l++) {
	    j = ix[l];
	    xj = x[j];
	    if (xj == 0.0)
		; // Relax.
	    else
		for (k = A->p[j]; k < A->p[j+1]; k++) {
		    aij   = A->x[k];
		    i     = A->i[k];
		    y[i] += aij * xj;
		}
	}
    }

    else if (mode == BCLS_PROD_At) {

	for (l = 0; l < nix; l++) {
	    j = ix[l];
	    sum = 0;
	    for (k = A->p[j]; k < A->p[j+1]; k++) {
		  aij  = A->x[k];
		  i    = A->i[k];
		  sum += aij * y[i];
	    }
	    x[j] = sum;
	}
    }

    // Exit.
    return 0;
}

// ---------------------------------------------------------------------
// Usolve
// Preconditioner.
//
// If     mode = BCLS_PRECON_U,  solve  U v = w,
// and if mode = BCLS_PRECON_Ut, solve  U'w = v.
// ---------------------------------------------------------------------
static int
Usolve( int mode, int m, int n, int nix, int ix[],
        double v[], double w[], void *UsrWrk ){
    
    assert( nix  <= n );
    assert( mode == BCLS_PRECON_U    ||
            mode == BCLS_PRECON_Ut   ||
            mode == BCLS_PRECON_INIT ||
            mode == BCLS_PRECON_TERM );
    
    if ( mode == BCLS_PRECON_INIT ) {
        // -------------------------------------------------------------
        // Initialize the preconditioner.
        // -------------------------------------------------------------

        // -------------------------------------------------------------
        // Factorize A(:,ix).
        // -------------------------------------------------------------


    }
    else if ( mode == BCLS_PRECON_U ) {
        
        // Solve  U v = w.

    }
    else if ( mode == BCLS_PRECON_Ut ) {

        // Solve  U'w = v.

    }
    else if ( mode == BCLS_PRECON_TERM )
        ; // Relax.
    
    // Exit.
    return 0;
}

// ---------------------------------------------------------------------
// CallBack.
// Periodically called by BCLS to test if the user wants to exit.
// ---------------------------------------------------------------------
static int
CallBack( BCLS *ls, void *UsrWrk )
{
    int err;
    err = 0;  // No error.
    return err;
}

// ---------------------------------------------------------------------
// Main wrapper routine to be called from fortran.
// 
// ---------------------------------------------------------------------
void bclsf90wrapper(int* mm, int* nn, int* nonz, int* ids, double* vals, int* nnzcol, double* b, double* c, double* mup, double* bl, double* bu, double* x)
{
    BCLS  *ls;               // A BCLS problem.
    worksp  Wrk;             // Workspace.
    int nnz;
    int err;
    int m, n, count;
    double damp;
    double* Xout  = NULL;    // output
    double *anorm = NULL;    // Column norms of A.
    int scaled_steepest = 0;
    int preconditioning = 0;
    int proj_search = 0;
    int newton_step = 0;   
    int i; 

    m = *mm;
    n = *nn;
    nnz = *nonz;
    damp = *mup;

    //x  = (double *)xmalloc( n, sizeof(double), "x"  );
    //dload(n,  0.0, x);
    // -----------------------------------------------------------------
    // Allocate storage for A, b, bl, bu, x, c.
    // Convert dense matrix A to its sparse representation
    // -----------------------------------------------------------------

    Wrk.A = cs_spalloc( m, n, nnz, 1, 0 );

    for(i = 0; i < n+1; i++){
        Wrk.A->p[i] = nnzcol[i];
    }

    for(i = 0; i < nnz; i++){
        Wrk.A->i[i] = ids[i];
        Wrk.A->x[i] = vals[i];
    }

    //count = 0;

    //Wrk.A->p[0] = count;
    //for(int i = 0; i < n; i++){
    //    for(int j = 0; j < m; j++){
    //        if(A[i*m+j] > 0.0){                
    //            Wrk.A->i[count] = j;
    //            Wrk.A->x[count] = A[i*m+j];
    //            count++;
    //        }
    //    }
    //    Wrk.A->p[i+1] = count;
    //} 

    // -----------------------------------------------------------------
    // Initialize a BCLS problem.  This routine MUST be called before
    // any other BCLS routine.
    // -----------------------------------------------------------------
    ls = bcls_create_prob( m, n );
    
    // Compute column norms.
    if ( scaled_steepest ) {

        // Allocate memory to hold the scales.
        anorm = xmalloc( n, sizeof(double), "anorm" );

        // Compute the scales and let BCLS know we have them.
        bcls_compute_anorm( ls, n, m, Aprod, &Wrk, anorm );
        bcls_set_anorm( ls, anorm );
    }

    // Instatiate a particular BCLS problem.
    bcls_set_problem_data( ls,    // The BCLS problem
			   m,     // Number of problem rows
			   n,     // Number of problem columns
			   Aprod, // The Mat-vec routine
			   &Wrk,  // Arbitrary data for the Mat-vec routine
			   damp,  // Damping parameter
			   x,     // Solution vector
			   b,     // RHS vector
			   c,     // Linear term (may be NULL)
			   bl,    // Lower-bounds vector
			   bu );  // Upper-bounds vector

    // initialize the options in the problem

    ls->print_level   =  2;
    if(proj_search){
        ls->proj_search    = BCLS_PROJ_SEARCH_EXACT;
    }
    else{
        ls->proj_search    = BCLS_PROJ_SEARCH_APPROX;
    }

    if(newton_step){
        ls->newton_step    = BCLS_NEWTON_STEP_CGLS;
    }
    else{
        ls->newton_step    = BCLS_NEWTON_STEP_LSQR;
    }

    ls->itnMajLim     = 5*ls->nmax;
    ls->itnMinLim     = 10*ls->nmax;

    ls->CallBack      = CallBack;

    if (preconditioning) bcls_set_usolve( ls, Usolve );

    // Call the main BCLS routine.
    err = bcls_solve_prob( ls );
     
    // Deallocate the BCLS problem.
    err = bcls_free_prob( ls );

    // Deallocate memory.
    //if ( b   ) free( b     );
    //if ( bl  ) free( bl    );
    //if ( bu  ) free( bu    );
    //if ( c   ) free( c     );
    if (anorm) free( anorm );
    
}

