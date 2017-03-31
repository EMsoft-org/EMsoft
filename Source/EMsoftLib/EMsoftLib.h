#ifndef _EMSOFTLIB_H_
#define _EMSOFTLIB_H_


#ifdef __cplusplus
extern "C" {
#endif


typedef void (*ProgCallBackType)(size_t, int);

typedef void (*ProgCallBackType2)(size_t, int, int, float);

typedef void (*ProgCallBackType3)(size_t, int, int, int, int);


/**
* EBSD pattern calculations:
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param EBSDpattern output array
* @param quats quaternion input array
* @param accum_e array with Monte Carlo histogram
* @param mLPNH Northern hemisphere master pattern
* @param mLPSH Southern hemisphere master pattern
* @param callback callback routine to update progress bar
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/

void EMsoftCgetEBSDPatterns
	(int32_t* ipar, float* fpar, float* EBSDpattern, 
	 float* quats, int32_t* accum_e, float* mLPNH, float* mLPSH,
         ProgCallBackType callback, size_t object, bool* cancel);


/**
* ECP calculations:
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param ECpattern output array
* @param quats quaternion input array
* @param accum_e array with Monte Carlo histogram
* @param mLPNH Northern hemisphere master pattern
* @param mLPSH Southern hemisphere master pattern
* @param callback callback routine to update progress bar
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/

void EMsoftCgetECPatterns
	(size_t* ipar, float* fpar, float* ECpattern, 
	 float* quats, float* accum_e, float* mLPNH, float* mLPSH,
         ProgCallBackType callback, size_t object, bool* cancel);

/**
* Monte Carlo calculations:
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param atompos atom position, site occupations and Debye-Waller factors
* @param atomtypes atom numbers
* @param latparm lattice parameters
* @param accum_e array with Monte Carlo energy histogram
* @param accum_z array with Monte Carlo depth histogram
* @param callback callback routine to update progress bar
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/


void EMsoftCgetMCOpenCL
        (int32_t* ipar, float* fpar, float* atompos, int32_t* atomtypes, 
        float* latparm, int32_t* accum_e, int32_t* accum_z, 
        ProgCallBackType2 callback, size_t object, bool* cancel);

/**
* EBSD master pattern calculations:
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param atompos atom position, site occupations and Debye-Waller factors
* @param atomtypes atom numbers
* @param latparm lattice parameters
* @param accum_z array with Monte Carlo depth histogram
* @param mLPNH modified Lambert projection northern hemisphere
* @param mLPSH modified Lambert projection southern hemisphere
* @param callback callback routine to update progress bar
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/


void EMsoftCgetEBSDmaster
        (int32_t* ipar, float* fpar, float* atompos, int32_t* atomtypes, 
        float* latparm, int32_t* accum_z,  float* mLPNH, float* mLPSH,
        ProgCallBackType3 callback, size_t object, bool* cancel);




#ifdef __cplusplus
}
#endif



#endif /*_EMSOFTLIB_H_*/
