/*
! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are 
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list 
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this 
!        list of conditions and the following disclaimer in the documentation and/or 
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
!        of its contributors may be used to endorse or promote products derived from 
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################
*/

typedef struct cs_sparse    /* matrix in compressed-column or triplet form */
{
    int nzmax ;	    /* maximum number of entries */
    int m ;	    /* number of rows */
    int n ;	    /* number of columns */
    int *p ;	    /* column pointers (size n+1) or col indices (size nzmax) */
    int *i ;	    /* row indices, size nzmax */
    double *x ;	    /* numerical values, size nzmax */
    int nz ;	    /* # of entries in triplet matrix, -1 for compressed-col */
} cs ;

cs *CalcBigACwrapper(char*);
static int Aprod( int , int , int , double* , double* , cs * );


/* BINDING ALL EXTERNAL FORTRAN FUNCTIONS */

extern void CalcBigA(char*, int*, int*, int*, int[], int[], double[]);
extern cs* CalcBigACwrapper(char*)

#define FORWARD_PROJECTION 0
#define BACK_PROJECTION    1
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <getopt.h>
//#include "cs.h"    // The CSparse matrix library.

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
Aprod( int mode, int m, int n,
       double x[], double y[], cs *BigA ) {

    int     i, j, k, l;
    double  aij, xj, sum;
    cs *A = BigA;

    assert( mode == FORWARD_PROJECTION     ||
            mode == BACK_PROJECTION        );
    assert( A->m == m );
    assert( A->n == n );

    if (mode == FORWARD_PROJECTION) {

        dload( m, 0.0, y );

	for (l = 0; l < n; l++) {
	    j = l;
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

    else if (mode == BACK_PROJECTION) {
	for (l = 0; l < n; l++) {
	    j = l;
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

cs *CalcBigACwrapper(char* NameList) {

    int     m;
    int     n;
    int     nnz;
    int*    nnzcolid;
    int*    nnzcol;
    double* val;
    int     i;

    // -----------------------------------------------------------------
    // Allocate storage for A, b, bl, bu, x, c.
    // Convert dense matrix A to its sparse representation
    // -----------------------------------------------------------------

    CalcBigA(NameList, &m, &n, &nnz, nnzcolid, nnzcol, val); 

    cs *BigA; 
    BigA = cs_spalloc( m, n, nnz, 1, 0 );

    for(i = 0; i < n+1; i++){
        BigA->p[i] = nnzcol[i];
    }

    for(i = 0; i < nnz; i++){
        BigA->i[i] = nnzcolid[i];
        BigA->x[i] = val[i];
    }

    return BigA;

}
