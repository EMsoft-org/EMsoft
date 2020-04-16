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
// EMsoft:EMMCfoil.cl
//--------------------------------------------------------------------------
//
// PROGRAM: EMMCfoil.cl
//
//> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
//
//> @brief OpenCL kernel for Monte Carlo BSE simulation
//
//> @detail Monte Carlo Electron Trajectory Simulation for EBSD
//>	This version uses the modified Lambert projection to store
//>	the MC output data, so that we are not dependent on a
//>	particular detector geometry.  We store the energy and direction
//>	cosines of a BSE electron along with depth information in the
//>	Lambert projection array, which needs to be sufficiently fine in
//>	terms of sampling so that we can deal with a detector that's relatively
//>	far away.
//> This is the part which runs on the GPU. This is independent of the platform. 
//
//> @date 01/14/16 MDG  1.0 based on EMMCsphere
//--------------------------------------------------------------------------

#define RAND_MAX  2147483647.0f
#define PI        3.14159f

struct LambertStruct{
    float x;
    float y;
};

struct lfsrret{
    int z1;
    int z2;
    int z3;
    int z4;
    int rand;
};

struct LambertStruct LambertSphereToPlane(float xyz[3]);
struct lfsrret lfsr113_Bits(int,int,int,int);

//--------------------------------------------------------------------------
//
// FUNCTION: rand_r
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief function to generate random number
//
//> @details this function generates a random number based on the seed value supplied. the random number generated serves as seed for the subsequent random number generated and so on.
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

//--------------------------------------------------------------------------
//
// FUNCTION: LambertSphereToPlane
//
//> @author Saransh Singh, Carnegie Mellon University
//
//> @brief Lambert projection of a point of the unit sphere
//
//> @param xyz 3D coordinates to be considered
//> @date 05/14/14    SS 1.0 original
//> @date 09/01/15   MDG 1.1 modified Lambert scaling to conform with Lambert module
//--------------------------------------------------------------------------

struct LambertStruct LambertSphereToPlane(float normxyz[3]){
    float q, LPssPi2, LPssPio2;
    LPssPi2 = 0.886226925452758f;
    LPssPio2 = 1.253314137315500f;
    struct LambertStruct ret;
    float xyz[3];
    float mag = sqrt(normxyz[0]*normxyz[0] + normxyz[1]*normxyz[1] + normxyz[2]*normxyz[2]);
    xyz[0] = normxyz[0]/mag;
    xyz[1] = normxyz[1]/mag;
    xyz[2] = normxyz[2]/mag;
    if(fabs(xyz[2]) == 1.0f){
        ret.x = 0.0f;
        ret.y = 0.0f;
    }
    else {
        if (fabs(xyz[1]) <= fabs(xyz[0]) && (xyz[0] != 0.0f)){
            q = (fabs(xyz[0])/xyz[0]) * sqrt(2.0f*(1.0f - fabs(xyz[2])));
            ret.x = q * LPssPi2;
            ret.y = q * atan(xyz[1]/xyz[0])/LPssPi2;
        }
        else{
            if (xyz[1] != 0.0f){
                q = (fabs(xyz[1])/xyz[1]) * sqrt(2.0f*(1.0f - fabs(xyz[2])));
                ret.x = q * atan(xyz[0]/xyz[1])/LPssPi2;
                ret.y = q * LPssPi2;
            }
        }
        
    }
    ret.x = ret.x/LPssPio2;
    ret.y = ret.y/LPssPio2;
    return ret;
    
}

//--------------------------------------------------------------------------
//
// FUNCTION: MC
//
//> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
//
//> @brief Kernel which does MC simulations for electron scattering
//
//> @details This is the OpenCL kernel which runs on the device and does the simulation. The original code has been modified to
//  study stochastic scattering from a thin foil. Distace unit in nm. origin at entrance plane of foil.
//
//> additional paramters--
//> thickness: thickness of the foil (in nm)
//> @date 10/20/16    SS 1.0 original
//> @date 01/15/17   MDG 1.1 modified for a thin foil
//--------------------------------------------------------------------------

__kernel void MC(const float E, const int count, const float z, const float rho, const float A, const int num_el, __global int* seeds, const float sig, const float omega, __global float* depth, __global float* energy, const int steps, const float thickness,__global float* LamxSH, __global float* LamySH)
{
    int tx, ty;
    tx = get_global_id(0);
    ty = get_global_id(1);
    float dir_cos[3];
    struct LambertStruct ret;
    
    int id = count*ty + tx;
    //int seed = seeds[id];
    float rand;
    int z11,z22,z33,z44;
    struct lfsrret retrnd;
    
    int counter1, counter2;
    float4 c_new, r_new;
    float E_new, alpha, de_ds, phi, psi, mfp,sig_eNA, step, dsq, dsqi, absc0z, th;
    th = thickness; // /1.e7
    
    float J;    // refer to Monte Carlo simulation for Electron Microscopy and Microanalysis, David C. Joy
    J = (9.76f*z + 58.5f*powr(z,-0.19f))*1E-3f;
    float xinit, zinit;
    xinit = 0.0f;
    zinit = 0.0f;

    float4 r0 = (float4)(xinit, 0.0f, -zinit, 0.0f);
    float4 c0 = (float4)(cos(omega)*sin(sig), sin(omega)*sin(sig), cos(sig), 0.0f);
    float escape_depth;

// Setting all values to -10. Any value other than -10 will denote a backscattered electron with the x and y component of the Lambert Projection

	for (int i = 0; i < num_el; ++i){
		LamxSH[num_el*id + i] = -10.0f;
		LamySH[num_el*id + i] = -10.0f;
        	depth[num_el*id + i] = 10.0f;
        	energy[num_el*id + i] = 0.0f;
	}

    
    for (int i = 0; i < num_el; ++i){
        z11 = seeds[4*id];
        z22 = seeds[4*id + 1];
        z33 = seeds[4*id + 2];
        z44 = seeds[4*id + 3];
        retrnd = lfsr113_Bits(z11,z22,z33,z44);
        seeds[4*id] = retrnd.z1;
        seeds[4*id + 1] = retrnd.z2;
        seeds[4*id + 2] = retrnd.z3;
        seeds[4*id + 3] = retrnd.z4;
        rand = fabs(retrnd.rand/RAND_MAX); //some random no. generator in gpu
        r0 = (float4)(xinit, 0.0f, -zinit, 0.0f);
// change the sign of the incidence angle since we're accumulating in the Southern hemisphere.
        c0 = (float4)(-cos(omega)*sin(sig), -sin(omega)*sin(sig), cos(sig), 0.0f);
        E_new = E;
        c_new = c0;
        
        alpha = (3.4E-3f)*powr(z,0.67f)/E_new;
        sig_eNA = (5.21f * 602.3f)*((z*z)/(E_new*E_new))*((4.0f*PI)/(alpha*(1+alpha)))*((E_new + 511.0f)*(E_new + 511.0f)/((E_new + 1024.0f)*(E_new + 1024.0f)));
        mfp = A/(rho*sig_eNA);
        step = -mfp * log(rand);
        r_new = r0 + step*c_new*1.0e7f;
        r0 = r_new;
        counter1 = 0;   // This is used as a counter for the number of monte carlo steps to carry out for each electron. This is due to the lock step nature of the GPU code. We have arbitrarily set this to a 300 or so, though this may be material dependent
        
        counter2 = 0;   // This counter is used to figure out if the electron has left the sample or not. Again, this is because of the lock step nature. All steps have to be executed on each thread irrespective of the fact that the electron may have actually left the sample

        while (counter1 < steps){
// inline code rather than function call
// Taken from book Monte Carlo simulation for Electron Microscopy and Microanalysis, David C. Joy

            alpha = (3.4E-3f)*powr(z,0.67f)/E_new;
            sig_eNA = (5.21f * 602.3f)*((z*z)/(E_new*E_new))*((4*PI)/(alpha*(1+alpha)))*((E_new + 511.0f)*(E_new + 511.0f)/((E_new + 1024.0f)*(E_new + 1024.0f)));
            mfp = A/(rho*sig_eNA);
            z11 = seeds[4*id];
            z22 = seeds[4*id + 1];
            z33 = seeds[4*id + 2];
            z44 = seeds[4*id + 3];
            retrnd = lfsr113_Bits(z11,z22,z33,z44);
            seeds[4*id] = retrnd.z1;
            seeds[4*id + 1] = retrnd.z2;
            seeds[4*id + 2] = retrnd.z3;
            seeds[4*id + 3] = retrnd.z4;
            rand = fabs(retrnd.rand/RAND_MAX); //some random no. generator in gpu
            step = -mfp * log(rand);
// This is the Continuous Slowing Down approximation 

            de_ds = -78500.0f*(z/(A*E_new)) * log(1.166f*E_new/J + 0.9911f);
            
            z11 = seeds[4*id];
            z22 = seeds[4*id + 1];
            z33 = seeds[4*id + 2];
            z44 = seeds[4*id + 3];
            retrnd = lfsr113_Bits(z11,z22,z33,z44);
            seeds[4*id] = retrnd.z1;
            seeds[4*id + 1] = retrnd.z2;
            seeds[4*id + 2] = retrnd.z3;
            seeds[4*id + 3] = retrnd.z4;
            rand = fabs(retrnd.rand/RAND_MAX);
            phi = acos(1 - ((2*alpha*rand)/(1 + alpha - rand)));

            z11 = seeds[4*id];
            z22 = seeds[4*id + 1];
            z33 = seeds[4*id + 2];
            z44 = seeds[4*id + 3];
            retrnd = lfsr113_Bits(z11,z22,z33,z44);
            seeds[4*id] = retrnd.z1;
            seeds[4*id + 1] = retrnd.z2;
            seeds[4*id + 2] = retrnd.z3;
            seeds[4*id + 3] = retrnd.z4;
            rand = fabs(retrnd.rand/RAND_MAX);
            psi = 2*PI*rand;
            
            
// new direction cosines of the electrons after scattering event
            if ((c0.z >= 0.99999f) || (c0.z <= -0.99999f) ){
                absc0z = fabs(c0.z);
                c_new = (float4)(sin(phi) * cos(psi), sin(phi) * sin(psi), (c0.z/absc0z)*cos(phi), 0.0f);
            }
            else {
                dsq = sqrt(1.0f-c0.z*c0.z);
                dsqi = 1.0f/dsq;
                c_new = (float4)(sin(phi)*(c0.x*c0.z*cos(psi) - c0.y*sin(psi))*dsqi + c0.x*cos(phi), sin(phi) * (c0.y * c0.z * cos(psi) + c0.x * sin(psi)) * dsqi + c0.y * cos(phi), -sin(phi) * cos(psi) * dsq + c0.z * cos(phi),0.0f);
            }

            if (fabs(c_new.z) > 1.0E-5f){
                escape_depth = (thickness - r_new.z)/c_new.z;
            }
            
            r_new = r0 + step*c_new*1.0e7f;
            r0 = r_new;
            c0 = c_new;
            E_new += step*rho*de_ds;

// we need to check if the electron has escaped or not; this is the part that depends on the sample shape...
// We do not keep track of electrons that have escaped on the positive z-side (BSEs) but we need to make
// sure that we eliminate them from all consideration
            if ( r0.z <= 0  && counter2 == 0) {
                counter2 = 1;
            }
            if ( r0.z >= thickness && counter2 == 0 ) {
                dir_cos[0] = c0.x;
                dir_cos[1] = c0.y;
                dir_cos[2] = c0.z;
                if (dir_cos[2] < 0.0f){
                    dir_cos[2] = -dir_cos[2];
                }
                if(dir_cos[0] != 0.0f && dir_cos[1] != 0.0f && dir_cos[2] != 0.0f){
                     ret = LambertSphereToPlane(dir_cos);
                     LamxSH[num_el*id + i] = ret.x;
                     LamySH[num_el*id + i] = ret.y;
                     depth[num_el*id + i] = escape_depth;
                     energy[num_el*id + i] = E_new;
                     counter2 = 1;
                }  
            }
			
            counter1++ ;
        }
        
    }

}
