// ###################################################################
// Copyright (c) 2014, Saransh Singh/Carnegie Mellon University
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
#define PI          3.14159265359f
#define TILE_SMALL  16
//--------------------------------------------------------------------------
// EMsoft:gamma.cl
// @brief OpenCL module for gamma-gamma' simulations
//
// @detail this module contains all the OpenCL functions for the gamma-gamma'
// image simulation. the CPU version uses a pre-computed list of scattering 
// matrices (based on Zassenhaus approximation), together with linear interpolation
// to compute the scattering matrices at any voxel. in the OpenCL implementation,
// we will relax these contraints and do the full simulation, with explicit
// calculations for each scattering matrix at every pixel. the 9th degree taylor 
// approximation will be used in performing matrix exponentiation.
//--------------------------------------------------------------------------

/*=====================================================================================
 ======================================================================================
 FUNCTIONS TO HANDLE COMPLEX NUMBERS IN THE OPENCL KERNEL
 ======================================================================================
 =====================================================================================*/

float2 cmplxmult(float2, float2);
float2 conjg(float2);
float2 cmplxexp(float);
float  sqrabs(float2);
float  calcsg(float [], float [], float []);
float  calcNorm(float []);
float2 ddelta(int, int, int);
//--------------------------------------------------------------------------
// FUNCTION: cmplxmult
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief function to calculate multiplication for two complex numbers
//
//> @date 09/23/14  SS  1.0 Original
//--------------------------------------------------------------------------

float2 cmplxmult(float2 a, float2 b){

    float2 res;
    res.x  =  a.x*b.x - a.y*b.y;
    res.y  =  a.x*b.y + a.y*b.x;
    
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
    ret   =   (float2)(a.x,-a.y);
    return ret;
}

//--------------------------------------------------------------------------
// FUNCTION: cmplxexp
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief function to calculate exponential of imaginary number
//
//> @date 06/15/15  SS  1.0 Original
//--------------------------------------------------------------------------

float2 cmplxexp(float a)
{
    float2 ret;
    ret   =   (float2)(native_cos(a),native_sin(a));
    return ret;
}

//--------------------------------------------------------------------------
// FUNCTION: sqrabs
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief function to calculate absolute suared value of a complex number
//
//> @date 06/17/15  SS  1.0 Original
//--------------------------------------------------------------------------

float sqrabs(float2 a)
{
    float ret;
    ret   =   a.x*a.x + a.y*a.y;
    return ret;
}

//----------------------------------------------------------------------------------
// FUNCTION: calcsg
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief function to calculate excitation error; ONLY valid for CARTESIAN frame
//> Note: FN should be normalised in the calling routine
//
//> the formula for the term sg is given by sg = -g.(2k + g)/(2|k + g|cos(alpha))
//> the calculation is only done in cartesian frame to avoid passing the direct
//> and reciprocal structure matrices. It can be done, but doesn't add much value 
//> to the code. These operations will be done inside the host code.
//> @date 11/29/17  SS  1.0 Original
//----------------------------------------------------------------------------------

float calcsg(float k0[3], float g[3], float FN[3])
{

// calculating (2k + g) and (k + g)
    float kpg[3], tkpg[3], xnom, xden, q1, q2, sg;
    for(int i = 0; i < 3; i++){
        kpg[i]   =   k0[i] + g[i];
        tkpg[i]  =  2.0f * k0[i] + g[i];
    }

    xnom  =  0.0f;
    xden  =  0.0f;
    q1    =  0.0f;
    q2    =  0.0f;

// xnom calculates -g.(2k + g)
// q1 calculates |k + g| while q2 calculates cos(alpha)
    for(int i = 0; i < 3; i++){
        xnom  +=  -g[i] * tkpg[i];
        q1    +=  kpg[i] * kpg[i];
        q2    +=  kpg[i] * FN[i];
    }

    q1   =  native_sqrt(q1);
    q2   =  q2/q1;
    xden =  2.0f * q1 * q2;

    sg   =  xnom/xden;

    return  sg;
}

//--------------------------------------------------------------------------
// FUNCTION: calcNorm
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief function to calculate 2-norm of a 3 dimensional vector in cartesian 
//  frame
//
//> @date 12/01/17  SS  1.0 Original
//--------------------------------------------------------------------------
float calcNorm(float k[3])
{
    float ret;
    ret = 0.0f;
    for(int i = 0; i < 3; i++){
        ret  +=  k[i] * k[i];
    }

    ret = native_sqrt(ret);
    return ret;
}

//--------------------------------------------------------------------------
// FUNCTION: ddelta
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief delta function calculator
//
//> @date 12/04/17  SS  1.0 Original
//--------------------------------------------------------------------------
float2 ddelta(int i, int j, int nref)
{
    float2 ret;
    ret = (float2)(0.0f,0.0f);
    if(i == j && i < nref) ret = (float2)(1.0f,0.0f);
    return ret;
}

//--------------------------------------------------------------------------
// EMsoft: dddSTEMDCI.cl
//--------------------------------------------------------------------------
//
// KERNEL: dddSTEMDCI
//
//> @author Saransh Singh/Joseph Tessmer, Carnegie Mellon University
//
//> @brief OpenCL kernel computing defect imaging using converged beam illumination
//  for gamma-gammap phase. 
//
//> @detail this opencl kernel takes the parameters for the converged probe and
//> calculates a full scattering matrix for each pixel. Depending on the location of
//> the detector, the image intensity will be calculated, but that is part of the
//> host code. In this routine, we also make the approximation that the change
//> in beam direction will only affect the geometry of the diffraction and not 
//> the beam interaction parameters. We will use the second order Zazzehaus 
//> approximation to calculate the scattering matrix
//
//> @param nsam         determines the sampling density of each CBED spot
//> @param nref         number of reflections considered
//> @param A_off        off diagonal term of the matrix 
//> @param A2           square of A_off
//> @param coef         complex coefficients for optimized taylor series
//> @param A3           cube of A_off
//> @param TT1          (A^3 - C0 * A2 + C1 * A - C2 * I)
//> @param TT2          (A^3 - C3 * A2 + C4 * A - C5 * I)
//> @param TT3          (A^3 - C6 * A2 + C7 * A - C8 * I)
//> @param tmp          temporary variable
//> @param SM           scattering matrix without geometrical factor (dependent of r)
//> @param A_dia        diagonal components of the matrix (dependent on k)
//> @param SMf          scattering matrix using second order zassenhaus approximation
//
//> @date 11/29/17  SS  1.0 original
//--------------------------------------------------------------------------
__kernel void dddSTEMDCI(             const     int         nsam,  // 0
                                      const     int         nref,  // 1
                                    __global    float2*     A_off, // 2
                                    __constant  float2*     coef,  // 3
                                    __global    float2*     A2,    // 4
                                    __global    float2*     A3,    // 5
                                    __global    float2*     TT1,   // 6
                                    __global    float2*     TT2,   // 7
                                    __global    float2*     TT3,   // 8
                                    __global    float2*     tmp,   // 9
                                    __global    float2*     SM,    // 10
                                    __global    float2*     A_dia, // 11
                                    __global    float2*     SMf,   // 12
                                    __global    float*      gmgpx, // 13
                                    __global    float*      gmgpy, // 14
                                    __global    float*      gmgpz, // 15
                                    __global    float2*     A_g,   // 16
                                      const     float       dx,    // 17
                                      const     float       dy,    // 18
                                      const     float       dz)    // 19

{

    int gx, gy, tid;
    int gsizex, gsizey;

    gsizex     =  get_global_size(0);
    gsizey     =  get_global_size(1);
    gx         =  get_global_id(0);
    gy         =  get_global_id(1);

    tid        =  gy * gsizex + gx;

    int lidx, lidy, lid;
    int lsizex, lsizey;

    lsizex     =  get_local_size(0);
    lsizey     =  get_local_size(1);
    lidx       =  get_local_id(0);
    lidy       =  get_local_id(1);

    lid        =  lidy * lsizex + lidx;

    float2 var;

    // Theta = exp((2*pi*i*g)*R(r))
    // this is the phase shift
    // we get the 2 pi from gmgp_
    // here g are the vectors between 
    //var        =   cmplxexp(-gmgpx[tid]*dx - gmgpy[tid]*dy - gmgpz[tid]*dz);
    var        =   cmplxexp(gmgpx[tid]*dx + gmgpy[tid]*dy + gmgpz[tid]*dz);

    // currently I have the same values in a_g and a_off
    A_off[tid]  =   cmplxmult(var, A_g[tid]);


    // if (tid == 100){
    // printf("%i %f %f %f %f %f %f %f %f\n", tid, var.x, var.y ,dx,dy,dz,gmgpx[tid],gmgpy[tid],gmgpz[tid]);
    // }


    barrier(CLK_GLOBAL_MEM_FENCE);

    // NOTE: We are assuming that the dynamical matrix will be symmetric
    // this loop is only correct in that case
    // otherwise the code needs to be changed
    // this has been implemented because the gamma-gamma' is centrosymmetric
    // this routine has been copied from the DictIndx.cl routine

    // Block index
    int bx      =  get_group_id(0);
    int by      =  get_group_id(1);
    
    // Thread index inside the block
    int tx      =  get_local_id(0);
    int ty      =  get_local_id(1);
    
    int aBegin  =  TILE_BIG * TILE_SMALL * by;
    int aEnd    =  aBegin + TILE_BIG - 1;
    int aStep   =  TILE_SMALL;
    
    int bBegin  =  TILE_SMALL * bx;
    //int bEnd;
    int bStep   =  TILE_SMALL * TILE_BIG;

    float2 Csub =  (float2)(0.0f, 0.0f);

    __local float2 As[TILE_SMALL][TILE_SMALL];
    __local float2 Bs[TILE_SMALL][TILE_SMALL];

    Csub =  (float2)(0.0f, 0.0f);

    for (int a = aBegin, b = bBegin; a <= aEnd; a += aStep, b += bStep){

            As[ty][tx] = A_off[a + TILE_BIG * ty + tx];
        
            Bs[ty][tx] = A_off[b + TILE_BIG * ty + tx];
       
            barrier(CLK_LOCAL_MEM_FENCE);
        
            #pragma unroll
            for (int k = 0; k < TILE_SMALL; ++k){
                Csub += cmplxmult(As[ty][k], Bs[k][tx]);
            }

            barrier(CLK_LOCAL_MEM_FENCE);
    }
    A2[get_global_id(1) * get_global_size(0) + get_global_id(0)] = Csub;
    barrier(CLK_GLOBAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

    Csub =  (float2)(0.0f, 0.0f);

    for (int a = aBegin, b = bBegin; a <= aEnd; a += aStep, b += bStep){

            As[ty][tx] = A2[a + TILE_BIG * ty + tx];
        
            Bs[ty][tx] = A_off[b + TILE_BIG * ty + tx];
        
            barrier(CLK_LOCAL_MEM_FENCE);
        
            #pragma unroll
            for (int k = 0; k < TILE_SMALL; ++k){
                Csub += cmplxmult(As[ty][k], Bs[k][tx]);
            }
            barrier(CLK_LOCAL_MEM_FENCE);
    }

    A3[get_global_id(1) * get_global_size(0) + get_global_id(0)] = Csub;
    barrier(CLK_GLOBAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

    float2 del = (float2)(0.0f, 0.0f);
    int id;

    id       =  tid;
    del      =  ddelta(gx, gy, nref);

    TT1[id]  =  A3[id] - cmplxmult(coef[0],A2[id]) + cmplxmult(coef[1],A_off[id]) - cmplxmult(coef[2],del);
    barrier(CLK_GLOBAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

    TT2[id]  =  A3[id] - cmplxmult(coef[3],A2[id]) + cmplxmult(coef[4],A_off[id]) - cmplxmult(coef[5],del);
    barrier(CLK_GLOBAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

    TT3[id]  =  A3[id] - cmplxmult(coef[6],A2[id]) + cmplxmult(coef[7],A_off[id]) - cmplxmult(coef[8],del);
    barrier(CLK_GLOBAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

    Csub =  (float2)(0.0f, 0.0f);

    for (int a = aBegin, b = bBegin; a <= aEnd; a += aStep, b += bStep){

        As[ty][tx] = TT2[a + TILE_BIG * ty + tx];
        
        Bs[ty][tx] = TT3[b + TILE_BIG * ty + tx];
        
        barrier(CLK_LOCAL_MEM_FENCE);
        
        #pragma unroll
        for (int k = 0; k < TILE_SMALL; ++k){
            Csub += cmplxmult(As[ty][k], Bs[k][tx]);
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        
    }
    tmp[get_global_id(1) * get_global_size(0) + get_global_id(0)] = Csub;
    barrier(CLK_GLOBAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

    Csub =  (float2)(0.0f, 0.0f);

    for (int a = aBegin, b = bBegin; a <= aEnd; a += aStep, b += bStep){

        As[ty][tx] = TT1[a + TILE_BIG * ty + tx];
        
        Bs[ty][tx] = tmp[b + TILE_BIG * ty + tx];
        
        barrier(CLK_LOCAL_MEM_FENCE);
        
        #pragma unroll
        for (int k = 0; k < TILE_SMALL; ++k){
            Csub += cmplxmult(As[ty][k], Bs[k][tx]);
        }
        barrier(CLK_LOCAL_MEM_FENCE);

    }
    SM[get_global_id(1) * get_global_size(0) + get_global_id(0)] = Csub;
    barrier(CLK_GLOBAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

    SM[tid]  =   SM[tid]/362880.0f;

    barrier(CLK_GLOBAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);

/*  the exponential calculation for the off-diagonal term needs to
    be integrated with the exponential of the diagonal term, which
    are dependent on the beam direction. this part is based on the 
    second order Zassenhaus approximation given by Van Dyck chapter: 
    exp(T+V) = exp(T/2)exp(V)exp(T/2) + O^2(T,V)                    */

    aBegin      =   0;
    aEnd        =   nsam * TILE_BIG;
    aStep       =   TILE_BIG;

    float2 e1   =   (float2)(0.0f, 0.0f);
    float2 e2   =   (float2)(0.0f, 0.0f);

    float2 Dsub =   (float2)(0.0f, 0.0f);

    for(int a = aBegin; a < aEnd; a += aStep){

        e1          =   A_dia[a + gx];
        e2          =   A_dia[a + gy];

        Dsub        =   SM[tid];

        Csub        =   cmplxmult(Dsub, e1);
        Dsub        =   cmplxmult(Csub, e2);
        
        SMf[a * TILE_BIG + tid] =   Dsub; 
    }                                             

} /* KERNEL FUNCTION ENDS HERE */

//--------------------------------------------------------------------------
// EMsoft:PropagateBeam.cl
//--------------------------------------------------------------------------
//
// KERNEL: PropagateBeam
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief OpenCL kernel to propagate the beam. 
//
// @detail this portion of the code performs the matrix vector operations
// to propagate the beam. we want to avoid using global memory as much 
// as possible, thus leaving each work group to perform the operations
// for each matrix row 
//
//> @param SM           all the scattering matrices for different beams
//> @param wavecoeffin  fourier coefficients of incoming wave
//> @param wavecoeffout fourier coefficients of outgoing wave
//> @param wavecoefftmp temporary variable
//> @param nsam         determines the sampling density of each CBED spot
//
//> @date 12/10/17  SS  1.0 original
//--------------------------------------------------------------------------
__kernel void PropagateBeam(    __global float2*    SM,
                                __global float2*    wavecoeffin,
                                __global float2*    wavecoeffout,
                                __global float2*    wavecoefftmp,
                                  const  int        nsam)
{

    int gx, gy, tid;
    int gsizex, gsizey;

    gsizex      =  get_global_size(0);
    gsizey      =  get_global_size(1);
    gx          =  get_global_id(0);
    gy          =  get_global_id(1);

    tid         =  gy * gsizex + gx;


    int offset_big, offset_small;

    int iBegin  =   0;
    int iEnd    =   (nsam * TILE_BIG)/ gsizey;
    int iStep   =   1;

    int stride  =   TILE_BIG * floor((float)gy/(float)TILE_BIG);

    float2 e1, e2, Csub;

// calculate individual products
    for(int i = iBegin; i < iEnd; i += iStep){

        offset_big          =   i * TILE_BIG * gsizey;
        offset_small        =   i * gsizey;

        e1                  =   SM[offset_big + tid];
        e2                  =   wavecoeffin[offset_small + stride + gx];

        Csub                =   cmplxmult(e1, e2);    

        wavecoefftmp[offset_big + tid] =   Csub;
    }

    barrier(CLK_GLOBAL_MEM_FENCE);

// sum up the row elements for the dot product
    for(int i = iBegin; i < iEnd; i += iStep){

        Csub                =   (float2)(0.0f, 0.0f);
        offset_big          =   i * TILE_BIG * gsizey;
        offset_small        =   i * gsizey;

        if(gx == 0){
            for(int k = 0; k < TILE_BIG; k++){
                Csub    +=  wavecoefftmp[offset_big + gy * gsizex + k];
            }
            wavecoeffout[offset_small + gy]   =  Csub;
        }
        //barrier(CLK_GLOBAL_MEM_FENCE);
    }
}

