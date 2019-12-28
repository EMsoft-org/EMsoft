/*
 ###################################################################
 ! Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
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
 !
 ! Code written by Saransh Singh, 2014.
 */

 #define PI 3.14159265359f
 #define e  2.71828182845f
 #define BLOCK_SIZE 16
 #define RAND_MAX  2147483647.0f


struct lfsrret{
    int z1;
    int z2;
    int z3;
    int z4;
    int rand;
};

float4 conjugate(float4);
float modulus(float4);
float4 quatmult(float4,float4);
float factorial(int);
float Bessel0(float);
float Bessel1(float);
float Bessel2(float);
float logCp(float);
float CalcVMF(float4,float4,float,float4);
float A4(float);
float FalsiMethod(float,float,float,int);
struct lfsrret lfsr113_Bits(int,int,int,int);
 /*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: conjugate
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to calculate conjugate of a quaternion
 //
 //> @param quat1 quaternion
 //> @date 01/11/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */

float4 conjugate(float4 quat)
{
    float4 ret;
    ret = (float4)(quat.x,-quat.y,-quat.z,-quat.w);
    return ret;
}

 /*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: modulus
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to calculate modulus of a quaternion
 //
 //> @param quat1 quaternion
 //> @date 01/12/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */

float modulus(float4 quat)
{
    float ret;
    ret = quat.x*quat.x + quat.y*quat.y + quat.z*quat.z + quat.w*quat.w;
    ret = sqrt(ret);
    return ret;
}

 /*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: quatmult
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to multiply two quaternions i.e. quat1 x quat2
 //
 //> @param quat1 first quaternion
 //> @param quat2 second quaternion
 //> @date 01/06/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */

 float4 quatmult(float4 quat1, float4 quat2)
{
    float4 res;
    res.x = quat1.x*quat2.x - quat1.y*quat2.y - quat1.z*quat2.z - quat1.w*quat2.w;
    res.y = quat1.y*quat2.x + quat1.x*quat2.y - quat1.w*quat2.z + quat1.z*quat2.w;
    res.z = quat1.z*quat2.x + quat1.w*quat2.y + quat1.x*quat2.z - quat1.y*quat2.w;
    res.w = quat1.w*quat2.x - quat1.z*quat2.y + quat1.y*quat2.z + quat1.x*quat2.w;
    
    return res;
}

 /*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: factorial
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to compute factorial of a function
 //
 //> @param x input integer parameter
 //> @date 01/06/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */

 float factorial(int x)
{
    int fact = 1;
    for (int i = 0; i <= x; ++i){
        fact *= i;
    }
    return fact;
}

 /*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: Bessel0
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to compute modified Bessel function of first kind of order 0
 //
 //> @param x input parameter
 //> @date 01/06/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */
float Bessel0(float x)
{
    float bessI0;
    float Y,AX,BX;
    
    float P1 = 1.0f;
    float P2 = 3.5156229f;
    float P3 = 3.0899424f;
    float P4 = 1.2067492f;
    float P5 = 0.2659732f;
    float P6 = 0.360768e-1f;
    float P7 = 0.45813e-2f;
    
    float Q1 = 0.39894228f;
    float Q2 = 0.1328592e-1f;
    float Q3 = 0.225319e-2f;
    float Q4 = 0.157565e-2f;
    float Q5 = 0.916281e-2f;
    float Q6 = -0.2057706e-1f;
    float Q7 = 0.2635537e-1f;
    float Q8 = -0.1647633e-1f;
    float Q9 = 0.392377e-2f;
    
    if (fabs(x) <= 3.75f){
        Y = (x/3.75f)*(x/3.75f);
        bessI0 = P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))));
    }
    else {
        AX = fabs(x);
        Y  = 3.75f/AX;
        BX = exp(AX)/sqrt(AX);
        AX = Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9)))))));
        bessI0 = AX*BX;
    }
    
    return bessI0;

}

 /*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: Bessel1
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to compute modified Bessel function of first kind of order 1
 //
 //> @param x input parameter
 //> @date 01/06/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */
float Bessel1(float x)
{
    float bessI1;
    float Y,AX,BX;
    
    float P1 = 0.5f;
    float P2 = 0.87890594f;
    float P3 = 0.51498869f;
    float P4 = 0.15084934f;
    float P5 = 0.2658733e-1f;
    float P6 = 0.301532e-2f;
    float P7 = 0.32411e-3f;
    
    float Q1 = 0.39894228f;
    float Q2 = -0.3988024e-1f;
    float Q3 = 0.362018e-2f;
    float Q4 = 0.163801e-2f;
    float Q5 = -0.1031555e-1f;
    float Q6 = 0.2282967e-1f;
    float Q7 = -0.2895312e-1f;
    float Q8 = 0.1787654e-1f;
    float Q9 = -0.420059e-2f;
    
    if (fabs(x) <= 3.75f){
        Y = (x/3.75f)*(x/3.75f);
        bessI1 = x*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))));
    }
    else {
        AX = fabs(x);
        Y  = 3.75f/AX;
        BX = exp(AX)/sqrt(AX);
        AX = Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9)))))));
        bessI1 = AX*BX;
    }
    
    return bessI1;
    
}
 /*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: Bessel2
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to compute modified Bessel function of first kind of order 2
 //
 //> @param x input parameter
 //> @date 01/06/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */
float Bessel2(float x)
{
    float res;
    if ( x >= 1.0e-5f){
        res = Bessel0(x) - (2/x)*Bessel1(x);
    }
    else {
        res = 0.0f;
    }
    return res;
}

/*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: A4
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to compute A4(u) = I2(u)/I1(u); Ip is the modified Bessel function of the first kind of order p
 //
 //> @param u input parameter
 //> @date 01/06/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */

float A4(float u)
{
    float ret;
    if (u <= 29.7487f && u >= 1.0e-5f){
        ret = Bessel2(u)/Bessel1(u);
    }
    else if (u > 29.7487f){
        ret = 1.0f + (24.0f*(5.0f - 8.0f*u))/(-15.0f + 16.0f*u*(8.0f*u - 3.0f));
    }
    else {
        ret = 0.0f;
    }
    return ret;
}

 /*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: logCp
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to generate logCp in the VMF distribution
 //
 //> @param kappa input value
 //> @date 01/06/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */
 float logCp(float kappa)
{
    float res;
    if (kappa >= 30.0f){
        res = pow(kappa,4.50f)/(-105.0f+8.0f*kappa*(-15.0f+16.0f*kappa*(-3.0f+8.0f*kappa)));
        res = 4.1746562059854348688f - kappa + log(res);
    }
    else {
        res = -3.675754132818690967f + log(kappa/Bessel1(kappa));
 
    }
    return res;
}

 /*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: CalcVMF
 //
 //> @author Saransh Singh, Carnegie Mellon University
 //
 //> @brief function to compute VMF density
 //
 //> @param quat unit quaternion
 //> @param mu mean value
 //> @param kappa concentartion parameter
 //> @date 01/06/15    SS 1.0 original
 //--------------------------------------------------------------------------
 */
 float CalcVMF(float4 quat, float4 mu, float kappa, float4 sym)
{
    float VMF;
    float4 meansym;
    float dp;
    meansym = quatmult(sym,mu);
    dp = meansym.x*quat.x + meansym.y*quat.y + meansym.z*quat.z + meansym.w*quat.w;
    VMF = exp(logCp(kappa)+kappa*dp);
    //pf = 2.0f*powr(kappa,2.5f)/sqrt(2.0f*PI)/(kappa - 1.0f);
    //VMF = pf*exp(kappa*(dp - 1.0f))*sqrt(1.0f - dp*dp);
    return VMF;
}

//--------------------------------------------------------------------------
//
// FUNCTION: lfsr113_Bits
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief function to generate random number
//
//> @details this function generates a random number based on the seed value supplied. the random number generated serves as seed for the subsequent random number generated and so on. this was taken from stacked overflow
//
//> @positive integer seed
//> @date 05/14/14    SS 1.0 original
//--------------------------------------------------------------------------
struct lfsrret lfsr113_Bits (int z11,int z22,int z33,int z44)
{
    struct lfsrret ret;
    int z1 = z11, z2 = z22, z3 = z33, z4 = z44;
    int b;
    b  = ((z1 << 6) ^ z1) >> 13;
    z1 = ((z1 & 4294967294U) << 18) ^ b;
    b  = ((z2 << 2) ^ z2) >> 27;
    z2 = ((z2 & 4294967288U) << 2) ^ b;
    b  = ((z3 << 13) ^ z3) >> 21;
    z3 = ((z3 & 4294967280U) << 7) ^ b;
    b  = ((z4 << 3) ^ z4) >> 12;
    z4 = ((z4 & 4294967168U) << 13) ^ b;
    ret.z1 = z1;
    ret.z2 = z2;
    ret.z3 = z3;
    ret.z4 = z4;
    ret.rand = (z1 ^ z2 ^ z3 ^ z4);
    return ret;
}

/*
 //--------------------------------------------------------------------------
 //
 // FUNCTION: FalsiMethod
 //
 //> @author taken from wikipedia
 //
 //> @brief function to find the root of the equation A4(kappa) - ||gamma||/n = 0
 //  where A4 is defined as A4(u) = I2(u)/I1(u); Ip is the modified Bessel function
 //  of the first kind of order p
 //
 //> @param u value of ||gamma||/n
 //--------------------------------------------------------------------------
 */

float FalsiMethod(float u, float s, float t, int m)
{
    float r,fr;
    int n, side=0;
    /* starting values at endpoints of interval */
    float fs = A4(s)-u;
    float ft = A4(t)-u;
    
    for (n = 0; n < m; n++)
    {
        
        r = (fs*t - ft*s) / (fs - ft);
        //if (fabs(t-s) < e*fabs(t+s)) break;
        fr = A4(r)-u;
        
        if (fr * ft > 0)
        {
            /* fr and ft have same sign, copy r to t */
            t = r; ft = fr;
            if (side==-1) fs /= 2;
            side = -1;
        }
        else if (fs * fr > 0)
        {
            /* fr and fs have same sign, copy r to s */
            s = r;  fs = fr;
            if (side==+1) ft /= 2;
            side = +1;
        }
        else
        {
            /* fr * f_ very small (looks like zero) */
            //break;
        } 
    }
    return r;
}

 /*
 !--------------------------------------------------------------------------
 !
 ! PROGRAM:ParamEstm
 !
 !> @author Saransh Singh, Carnegie Mellon University
 !
 !> @brief perform parameter estimation for the dictionary indexing
 !
 !> @param quaternion list of quaternions for which parameter estimation is done
 !> @param mean output variable containing the mean values i.e. mu
 !> @param kappa output variable containing the concentration parameter
 !> @param symmetry the list of symmetry operators
 !> @param numk number of quaternion from which mean is calculated
 !> @param numsym number of symmetry elements in the symmetry matrix
 !
 !> @date 01/06/15  SS 1.0 original
 !--------------------------------------------------------------------------
 */

__kernel void ParamEstm(__global float* quaternion, __global float* mean, __global float* ConcParam, __global float* symmetry, const int numk, const int numsym,__global float* lookup,__global int* seeds,const int numinit)
{
    int tx = get_global_id(0);
    int ty = get_global_id(1);
    
    int id = get_global_size(0)*ty + tx;
    
    float4 quatlist;
    
    float4 gamma;
    float modgamma;
    float prefact;
    
    float4 mu = (float4)(0.0f,0.0f,0.0f,0.0f);
    float kappa;//,spl;
    
    //float rand;
    //int z11,z22,z33,z44;
    //struct lfsrret retrnd;
    
    float xvar = 0.0f;
    float4 sym;
    float rim;
    float val,min,Lintd;
    float Lnew,Lold;
    float4 munew;
    float kappanew;
    Lold = 0.0f;
    kappanew = 0.0f;
    munew = (float4)(0.0f,0.0f,0.0f,0.0f);
    kappa = 0.0f;
    
    //z11 = seeds[4*id];
    //z22 = seeds[4*id + 1];
    //z33 = seeds[4*id + 2];
    //z44 = seeds[4*id + 3];

    
    for (int p = 0; p < numinit; ++p){
        Lintd = 0.0f;
        /*
        retrnd = lfsr113_Bits(z11,z22,z33,z44);
        rand = fabs(retrnd.rand/RAND_MAX);
        mu.x = rand*10.0f;
        
        z11 = retrnd.z1;
        z22 = retrnd.z2;
        z33 = retrnd.z3;
        z44 = retrnd.z4;
        
        retrnd = lfsr113_Bits(z11,z22,z33,z44);
        rand = fabs(retrnd.rand/RAND_MAX);
        mu.y = rand*10.0f;
        
        z11 = retrnd.z1;
        z22 = retrnd.z2;
        z33 = retrnd.z3;
        z44 = retrnd.z4;
        
        retrnd = lfsr113_Bits(z11,z22,z33,z44);
        rand = fabs(retrnd.rand/RAND_MAX);
        mu.z = rand*10.0f;
        
        z11 = retrnd.z1;
        z22 = retrnd.z2;
        z33 = retrnd.z3;
        z44 = retrnd.z4;
        
        retrnd = lfsr113_Bits(z11,z22,z33,z44);
        rand = fabs(retrnd.rand/RAND_MAX);
        mu.w = rand*10.0f;
        
        z11 = retrnd.z1;
        z22 = retrnd.z2;
        z33 = retrnd.z3;
        z44 = retrnd.z4;
        
        retrnd = lfsr113_Bits(z11,z22,z33,z44);
        rand = fabs(retrnd.rand/RAND_MAX);
        kappa = 200.0f + 100.0f*rand;

        z11 = retrnd.z1;
        z22 = retrnd.z2;
        z33 = retrnd.z3;
        z44 = retrnd.z4;*/
        
        mu = (float4){2.0f,1.0f,3.0f,0.0f};
        kappa = 100.0f;
        mu = mu/modulus(mu);
        for (int l = 0; l < 20; ++l){
            gamma = (float4)(0.0f,0.0f,0.0f,0.0f);
            for (int i = 0; i < numk; ++i){
                quatlist = (float4)(quaternion[4*id*numk+i],quaternion[4*id*numk+i+1],quaternion[4*id*numk+i+2],quaternion[4*id*numk+i+3]);
                prefact = 0.0f;
                for (int j = 0; j < numsym; ++j){
                    sym = (float4)(symmetry[4*j],symmetry[4*j+1],symmetry[4*j+2],symmetry[4*j+3]);
                    prefact += CalcVMF(quatlist,mu,kappa,sym);
                }
                
                for (int k = 0; k < numsym; ++k){
                    sym = (float4)(symmetry[4*k],symmetry[4*k+1],symmetry[4*k+2],symmetry[4*k+3]);
                    rim = CalcVMF(quatlist,mu,kappa,sym)/prefact;
                    gamma += rim*quatmult(quatlist,conjugate(sym));
                    
                }
            }
            
            modgamma = modulus(gamma);
            
            mu = gamma/modgamma;
            
            xvar = modgamma/(float)numk;
            if (xvar >=0.95f){
                kappa = (-15.0f + 3.0f*xvar - 1.73205081f*sqrt(5.0f + 30.0f*xvar + 13.0f*xvar*xvar))/(16.0f*(xvar - 1.0f));
            }
            else {
                min = 100.0f;
                for (int m = 0; m < 590; ++m){
                    val = fabs(xvar - lookup[m]);
                    if (val <= min){
                        min = val;
                        kappa = 0.05f*(m+1);
                    }
                }
            }
        }
        
        for (int i = 0; i < numk; ++i){
            quatlist = (float4)(quaternion[4*id*numk+i],quaternion[4*id*numk+i+1],quaternion[4*id*numk+i+2],quaternion[4*id*numk+i+3]);
            prefact = 0.0f;
            for (int j = 0; j < numsym; ++j){
                sym = (float4)(symmetry[4*j],symmetry[4*j+1],symmetry[4*j+2],symmetry[4*j+3]);
                prefact += CalcVMF(quatlist,mu,kappa,sym);
            }
            Lintd += log(prefact);
        }
        Lnew = Lintd;
        if (Lnew >= Lold){
            kappanew = kappa;
            munew = mu;
            Lold = Lnew;
        }

    }
    
    //pos = maxpos(L,numinit);
    mean[4*id] = munew.x;
    mean[4*id + 1] = munew.y;
    mean[4*id + 2] = munew.z;
    mean[4*id + 3] = munew.w;
    ConcParam[id] = kappanew;
    
}
/*
!--------------------------------------------------------------------------
!
! PROGRAM:InnerProd
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief perform inner product calculations for normalized images as a matrix multiplication. Based on the nVidia SDK code https://developer.nvidia.com/opencl
!
!> @param exp experimental pattern chunk
!> @param dict dictionary pattern chunk
!> @param L size of one image in pixels
!> @param Ne number of experimental patterns in one chunk
!> @param Nd number of dictionary patterns in one chunk
!> @param result result of the dot product
!
!> @date 12/09/14  SS 1.0 original
!--------------------------------------------------------------------------
*/

//#define AS(i,j) As[i * 64 + j]
//#define BS(i,j) Bs[i * 64 + j]

// Notice that the Block Size is pre-set to 16. This needs to maybe become more flexible in the future.

__kernel void InnerProd(__global float* expt, __global float* dict, int Wexp, int Wdict, __global float* result)
{
    // Block index
    int bx = get_group_id(0);
    int by = get_group_id(1);
    
    // Thread index inside the block
    int tx = get_local_id(0);
    int ty = get_local_id(1);
    
    int aBegin = Wexp * BLOCK_SIZE * by;
    int aEnd = aBegin + Wexp - 1;
    int aStep = BLOCK_SIZE;
    
    int bBegin = BLOCK_SIZE * bx;
    //int bEnd = bBegin + Wdict * (get_num_groups(1));
    int bstep = BLOCK_SIZE * Wdict;
    float Csub = 0.0f;
    
    __local float As[BLOCK_SIZE][BLOCK_SIZE];
    __local float Bs[BLOCK_SIZE][BLOCK_SIZE];
    
    for (int a = aBegin, b = bBegin; /*b <= bEnd*/a <= aEnd; a += aStep, b += bstep){
        
        As[ty][tx] = expt[a + Wexp * ty + tx];
        
        Bs[ty][tx] = dict[b + Wdict * ty + tx];
        
        barrier(CLK_LOCAL_MEM_FENCE);
        
        #pragma unroll
        for (int k = 0; k < BLOCK_SIZE; ++k){
            Csub += As[ty][k] * Bs[k][tx];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    //int c = Wdict * BLOCK_SIZE * by + BLOCK_SIZE * bx;
    result[get_global_id(1) * get_global_size(0) + get_global_id(0)] = Csub;
    //result[get_global_id(1) * get_global_size(0) + get_global_id(0)] = float(tx);
    //result[c + Wdict*ty + tx] = Csub;
    
}
