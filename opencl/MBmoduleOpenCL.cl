// ###################################################################
// Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without modification, are
// permitted provided that the following conditions are met:
//
//     - Redistributions of source code must retain the above copyright notice, this list
//        of conditions and the following disclaimer.
//     - Redistributions in binary form must reproduce the above copyright notice, this
//        list of conditions and the following disclaimer in the documentation and/or
//        other materials provided with the distribution.
//     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names
//        of its contributors may be used to endorse or promote products derived from
//        this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
// USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// ###################################################################

//--------------------------------------------------------------------------
// EMsoft:MBmoduleOpenCL.cl
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
// FUNCTION: cmplxmult
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief function to calculate multiplication for two compllex numbers
//
//> @date 09/23/14  SS  1.0 Original
//--------------------------------------------------------------------------

float2 cmplxmult(float2, float2);
float2 conjg(float2);

float2 cmplxmult(float2 a, float2 b){
    float2 res;
    res.x = a.x*b.x - a.y*b.y;
    res.y = a.x*b.y + a.y*b.x;
    
    return res;
}

//--------------------------------------------------------------------------
//
// FUNCTION: conjg
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief function to calculate conjugate of a compllex number
//
//> @date 02/19/15  SS  1.0 Original
//--------------------------------------------------------------------------

float2 conjg(float2 a)
{
    float2 ret;
    ret = (float2)(a.x,-a.y);
    return ret;
}

//--------------------------------------------------------------------------
// EMsoft:ScatMat.cl
//--------------------------------------------------------------------------
//
// KERNEL: ScatMat
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief OpenCL kernel for calculating exponential of matrix using Taylor series when
//  multiple exponentials are calculated simultaneously. This routine is for the TEM
//  geometry.
//
//> @date 09/09/14  SS  1.0 Original
//> @date 02/16/15  SS  1.1 bug fixes and simplification using cmplxmult
//--------------------------------------------------------------------------

__kernel void ScatMat(__global float2* cl_expA,
                      __global float2* cl_A,
                      __global float2* cl_AA,
                      __global float2* cl_AAA,
                      __global int* arrsize,
                      __global float2* cl_coeff,
                      __global float2* cl_T1,
                      __global float2* cl_T2,
                      __global float2* cl_wavefncoeff,
                      __global float2* cl_wavefncoeffintd,
                      __global int* sqrsize,
                      __global int* offset,
                      __global int* arrsizesum,
                      __global int* numdepth)
{
    int tx,ty,id;
    tx = get_global_id(0);
    ty = get_global_id(1);
    id = get_global_size(0)*ty + tx;
    
    float2 sum = (float2)(0.0f,0.0f);
    int nn = arrsize[id];
    int off = offset[id];
    int off2 = arrsizesum[id];
    int ns = sqrsize[id];
    // calculating A^2 and A^3
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_A[off + i*nn + k],cl_A[off + k*nn + j]);
            }
            cl_AA[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }

    
    sum = (float2)(0.0f,0.0f);
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_AA[off + i*nn + k],cl_A[off + k*nn + j]);
            }
            cl_AAA[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    // Calculating the three factors for the exponential
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            if ( i == j){
                cl_expA[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[0],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[1],cl_A[off + i*nn + j]) - cl_coeff[2];
            
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[3],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[4],cl_A[off + i*nn + j]) - cl_coeff[5];

            }
            else {
                
                cl_expA[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[0],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[1],cl_A[off + i*nn + j]);
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[3],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[4],cl_A[off + i*nn + j]);
                
                
            }
        }
    }

    
    sum = (float2)(0.0f,0.0f);
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_T1[off + i*nn + k],cl_expA[off + k*nn + j]);
            }
            cl_T2[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            if ( i == j){
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[6],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[7],cl_A[off + i*nn + j]) - cl_coeff[8];
                
            }
            else {
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[6],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[7],cl_A[off + i*nn + j]);
                
            }
        }
    }


    sum = (float2)(0.0f,0.0f);
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_T2[off + i*nn + k],cl_T1[off + k*nn + j]);
            }
            sum /= 362880;
            cl_expA[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    // squaring operation as matrix was scaled in the host code
    
    for (int l = 0; l < ns; l++){
        sum = (float2)(0.0f,0.0f);
        for (int i = 0; i < nn; i++){
            for (int j = 0; j < nn; j++){
                for (int k = 0; k < nn; k++){
                    
                    sum += cmplxmult(cl_expA[off + i*nn + k],cl_expA[off + k*nn + j]);

                }
                cl_T1[off + i*nn + j] = sum;
                sum = (float2)(0.0f,0.0f);
            }
        }

        for (int i = 0; i < nn; i++){
            for (int j = 0; j < nn; j++){
                cl_expA[off + i*nn + j] = cl_T1[off + i*nn + j];
            }
        }
    }

    // cl_expA now has the exponential of the structure matrix. We now multiply
    // by the column vector [1 0 0 ...... 0] to the the fourier coefficients of
    // wavefunction at different depths and subsequently the depth integrated
    // intensity
    

    
    for (int i = 0; i < nn; i++){
        
        if ( i == 0) {
            cl_wavefncoeff[off2 + i] = (float2)(1.0f,0.0f);
        }
        
        else {
            cl_wavefncoeff[off2 + i] = (float2)(0.0f,0.0f);
        }
        
    }
    
    int nstep = numdepth[id];
  
    
    for (int i = 0; i < nstep; i++){
        
        sum = (float2)(0.0f,0.0f);
        
        for (int l = 0; l < nn; l++){
            cl_wavefncoeffintd[off2 + l] = cl_wavefncoeff[off2 + l];
        }
        
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                
                sum += cmplxmult(cl_expA[off + k*nn + j],cl_wavefncoeffintd[off2 + k]);

            }
            
            cl_wavefncoeff[off2 + j] = sum;
            sum = (float2)(0.0f,0.0f);
            
        }
        
    }
    
    // we have the fourier coefficients of the wavefunctions.
}

//--------------------------------------------------------------------------
// EMsoft:CalcLgh.cl
//--------------------------------------------------------------------------
//
// KERNEL: CalcLgh
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief OpenCL kernel for calculating exponential of matrix using Taylor series when
//  multiple exponentials are calculated simultaneously. This routine is for the SEM
//  geometry.
//
//> @date 02/19/15  SS  1.0 Original
//--------------------------------------------------------------------------

__kernel void CalcLgh(__global float2* cl_expA,
                      __global float2* cl_A,
                      __global float2* cl_AA,
                      __global float2* cl_AAA,
                      __global int* arrsize,
                      __global float2* cl_coeff,
                      __global float2* cl_T1,
                      __global float2* cl_T2,
                      __global float2* cl_wavefncoeff,
                      __global float2* cl_wavefncoeffintd,
                      __global int* sqrsize,
                      __global int* offset,
                      __global int* arrsizesum,
                      const int numdepth,
                      __global float* lambdas)
{
    int tx,ty,id;
    tx = get_global_id(0);
    ty = get_global_id(1);
    id = get_global_size(0)*ty + tx;
    
    float2 sum = (float2)(0.0f,0.0f);
    int nn = arrsize[id];
    int off = offset[id];
    int off2 = arrsizesum[id];
    int ns = sqrsize[id];
    // calculating A^2 and A^3
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_A[off + i*nn + k],cl_A[off + k*nn + j]);
            }
            cl_AA[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    
    sum = (float2)(0.0f,0.0f);
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_AA[off + i*nn + k],cl_A[off + k*nn + j]);
            }
            cl_AAA[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    // Calculating the three factors for the exponential
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            if ( i == j){
                cl_expA[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[0],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[1],cl_A[off + i*nn + j]) - cl_coeff[2];
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[3],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[4],cl_A[off + i*nn + j]) - cl_coeff[5];
                
            }
            else {
                
                cl_expA[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[0],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[1],cl_A[off + i*nn + j]);
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[3],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[4],cl_A[off + i*nn + j]);
                
                
            }
        }
    }
    
    
    sum = (float2)(0.0f,0.0f);
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_T1[off + i*nn + k],cl_expA[off + k*nn + j]);
            }
            cl_T2[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            if ( i == j){
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[6],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[7],cl_A[off + i*nn + j]) - cl_coeff[8];
                
            }
            else {
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[6],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[7],cl_A[off + i*nn + j]);
                
            }
        }
    }
    
    
    sum = (float2)(0.0f,0.0f);
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_T2[off + i*nn + k],cl_T1[off + k*nn + j]);
            }
            sum /= 362880;
            cl_expA[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    // squaring operation as matrix was scaled in the host code
    
    for (int l = 0; l < ns; l++){
        sum = (float2)(0.0f,0.0f);
        for (int i = 0; i < nn; i++){
            for (int j = 0; j < nn; j++){
                for (int k = 0; k < nn; k++){
                    
                    sum += cmplxmult(cl_expA[off + i*nn + k],cl_expA[off + k*nn + j]);
                    
                }
                cl_T1[off + i*nn + j] = sum;
                sum = (float2)(0.0f,0.0f);
            }
        }
        
        for (int i = 0; i < nn; i++){
            for (int j = 0; j < nn; j++){
                cl_expA[off + i*nn + j] = cl_T1[off + i*nn + j];
                cl_T1[off + i*nn + j] = (float2)(0.0f,0.0f);
            }
        }
    }
    
    // cl_expA now has the exponential of the structure matrix. We now multiply
    // by the column vector [1 0 0 ...... 0] to the the fourier coefficients of
    // wavefunction at different depths and subsequently the depth integrated
    // intensity
    
    
    
    for (int i = 0; i < nn; i++){
        
        if ( i == 0) {
            cl_wavefncoeff[off2 + i] = (float2)(1.0f,0.0f);
        }
        
        else {
            cl_wavefncoeff[off2 + i] = (float2)(0.0f,0.0f);
        }
        
    }
    
    int nstep = numdepth;
    
    for (int i = 0; i < nstep; i++){
        
        sum = (float2)(0.0f,0.0f);
        
        for (int l = 0; l < nn; l++){
            cl_wavefncoeffintd[off2 + l] = cl_wavefncoeff[off2 + l];
        }
        
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                
                sum += cmplxmult(cl_expA[off + k*nn + j],cl_wavefncoeffintd[off2 + k]);
                
            }
            
            cl_wavefncoeff[off2 + j] = sum;
            sum = (float2)(0.0f,0.0f);
            
        }
        
        // we have the fourier coefficients of the wavefunctions at depth i now. using the lambda values, we can compute the Lgh matrix now
        
        for (int m = 0; m < nn ; m++){
            for (int p = 0; p < nn; p++){
                cl_T1[off + m*nn + p] += lambdas[i]*cmplxmult(cl_wavefncoeff[off2 + m],conjg(cl_wavefncoeff[off2 + p]));

            }
        }
 
        
    }
    
   // we now have the Lgh matrix for depth integrated intensity calculations
   // ready to quit code
    
    
}

//--------------------------------------------------------------------------
// EMsoft:CalcLghMaster.cl
//--------------------------------------------------------------------------
//
// KERNEL: CalcLghMaster
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief OpenCL kernel for calculating exponential of matrix using Taylor
//> series when multiple exponentials are calculated simultaneously. This
//> subroutine makes an approximation that the small patch of pixels in the
//> lambert space have the foil normals very close to each other and thus the
//> dynamical matrices will be identical, except for the sg values, which are
//> the diagonal entries of the matrix.
//
//> @date 02/19/15  SS  1.0 Original
//--------------------------------------------------------------------------

__kernel void CalcLghMaster(__global float2* cl_expA,
                            __global float2* cl_offdiagonal,
                            __global float2* cl_diagonal,
                            __global float2* cl_AA,
                            __global float2* cl_AAA,
                            __global float2* cl_coeff,
                            __global float2* cl_T1,
                            __global float2* cl_T2,
                            __global float2* cl_wavefncoeff,
                            __global float2* cl_wavefncoeffintd,
                            const int nn,
                            __global int* sqrsize,
                            const int numdepth,
                            __global float* lambdas,
                            __global float2* cl_A)
{
    int tx,ty,id;
    tx = get_global_id(0);
    ty = get_global_id(1);
    id = get_global_size(0)*ty + tx;
    
    float2 sum = (float2)(0.0f,0.0f);
    int off = id*nn*nn;
    int off2 = id*nn;
    int ns = sqrsize[id];
    
    // fillingin the A matrix
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            if (i != j){
                cl_A[off + i*nn + j] = cl_offdiagonal[i*nn + j]/powr(2.0f,ns);
            }
            
        }
        cl_A[off + i*(nn+1)] = cl_diagonal[off2 + i];

    }
    
    
  
    // calculating A^2 and A^3
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_A[off + i*nn + k],cl_A[off + k*nn + j]);
            }
            cl_AA[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    sum = (float2)(0.0f,0.0f);
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_AA[off + i*nn + k],cl_A[off + k*nn + j]);
            }
            cl_AAA[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    // Calculating the three factors for the exponential
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            if ( i == j){
                cl_expA[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[0],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[1],cl_A[off + i*nn + j]) - cl_coeff[2];
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[3],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[4],cl_A[off + i*nn + j]) - cl_coeff[5];
                
            }
            else {
                
                cl_expA[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[0],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[1],cl_A[off + i*nn + j]);
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[3],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[4],cl_A[off + i*nn + j]);
                
                
            }
        }
    }
    
    
    sum = (float2)(0.0f,0.0f);
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_T1[off + i*nn + k],cl_expA[off + k*nn + j]);
            }
            cl_T2[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            if ( i == j){
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[6],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[7],cl_A[off + i*nn + j]) - cl_coeff[8];
                
            }
            else {
                
                cl_T1[off + i*nn + j] = cl_AAA[off + i*nn + j] - cmplxmult(cl_coeff[6],cl_AA[off + i*nn + j]) + cmplxmult(cl_coeff[7],cl_A[off + i*nn + j]);
                
            }
        }
    }
    
    
    sum = (float2)(0.0f,0.0f);
    for (int i = 0; i < nn; i++){
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                sum += cmplxmult(cl_T2[off + i*nn + k],cl_T1[off + k*nn + j]);
            }
            sum /= 362880;
            cl_expA[off + i*nn + j] = sum;
            sum = (float2)(0.0f,0.0f);
        }
    }
    
    // squaring operation as matrix was scaled in the host code
    
    for (int l = 0; l < ns; l++){
        sum = (float2)(0.0f,0.0f);
        for (int i = 0; i < nn; i++){
            for (int j = 0; j < nn; j++){
                for (int k = 0; k < nn; k++){
                    
                    sum += cmplxmult(cl_expA[off + i*nn + k],cl_expA[off + k*nn + j]);
                    
                }
                cl_T1[off + i*nn + j] = sum;
                sum = (float2)(0.0f,0.0f);
            }
        }
        
        for (int i = 0; i < nn; i++){
            for (int j = 0; j < nn; j++){
                cl_expA[off + i*nn + j] = cl_T1[off + i*nn + j];
                cl_T1[off + i*nn + j] = (float2)(0.0f,0.0f);
            }
        }
    }
    
    // cl_expA now has the exponential of the structure matrix. We now multiply
    // by the column vector [1 0 0 ...... 0] to the the fourier coefficients of
    // wavefunction at different depths and subsequently the depth integrated
    // intensity
    
    
    
    for (int i = 0; i < nn; i++){
        
        if ( i == 0) {
            cl_wavefncoeff[off2 + i] = (float2)(1.0f,0.0f);
        }
        
        else {
            cl_wavefncoeff[off2 + i] = (float2)(0.0f,0.0f);
        }
        
    }
    
    int nstep = numdepth;
    
    for (int i = 0; i < nstep; i++){
        
        sum = (float2)(0.0f,0.0f);
        
        for (int l = 0; l < nn; l++){
            cl_wavefncoeffintd[off2 + l] = cl_wavefncoeff[off2 + l];
        }
        
        for (int j = 0; j < nn; j++){
            for (int k = 0; k < nn; k++){
                
                sum += cmplxmult(cl_expA[off + k*nn + j],cl_wavefncoeffintd[off2 + k]);
                
            }
            
            cl_wavefncoeff[off2 + j] = sum;
            sum = (float2)(0.0f,0.0f);
            
        }
        
        // we have the fourier coefficients of the wavefunctions at depth i now. using the lambda values, we can compute the Lgh matrix now
        
        for (int m = 0; m < nn ; m++){
            for (int p = 0; p < nn; p++){
                cl_T1[off + m*nn + p] += lambdas[i]*cmplxmult(cl_wavefncoeff[off2 + m],conjg(cl_wavefncoeff[off2 + p]));
                
            }
        }
        
        
    }
    
    // we now have the Lgh matrix for depth integrated intensity calculations
    // ready to quit code
    

}
