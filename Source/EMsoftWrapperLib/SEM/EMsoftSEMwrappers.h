#ifndef _emsoft_SEMwrappers_H_
#define _emsoft_SEMwrappers_H_


#ifdef __cplusplus
extern "C" {
#endif

/**
* @brief This is the typedef for a call back function that is
* used in the EMsoft library.
* @param size_t Unique integer that designates which C++ object
* did the call into EMsoft
* @param int
*/
typedef void (*ProgCallBackType)(size_t, int);

/**
* @brief This is the typedef for a call back function that is
* used in the EMsoft library.
* @param size_t Unique integer that designates which C++ object
* did the call into EMsoft
* @param int
* @param int
* @param float
*/
typedef void (*ProgCallBackType2)(size_t, int, int, float);

/**
* @brief This is the typedef for a call back function that is
* used in the EMsoft library.
* @param size_t Unique integer that designates which C++ object
* did the call into EMsoft
* @param int
* @param int
* @param int
* @param int
*/
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
* @param spar array with string parameters
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
        (int32_t* ipar, float* fpar, char* spar, float* atompos, int32_t* atomtypes, 
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

/**
* EBSD dynamical reflector ranking:
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param atompos atom position, site occupations and Debye-Waller factors
* @param atomtypes atom numbers
* @param latparm lattice parameters
* @param accum_e array with Monte Carlo energy histogram
* @param mLPNH modified Lambert projection northern hemisphere
* @param mLPSH modified Lambert projection southern hemisphere
* @param hkl list of hkl triplets (output)
* @param beta list of dynamical integrated Kikuchi band intensities (output)
* @param XKI list of x-ray kinematical intensities (output)
* @param EKI list of electron kinematical intensities (output)
* @param callback callback routine to update progress bar
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/
void EMsoftCgetEBSDreflectorranking
        (int32_t* ipar, float* fpar, float* atompos, int32_t* atomtypes, 
        float* latparm, int32_t* accum_e, float* mLPNH, float* mLPSH,
        int32_t* hkl, float* beta, float* XKI, float* EKI, 
        ProgCallBackType2 callback, size_t object, bool* cancel);

/**
 * @brief HiPassFilterC
 * @param rdata real data to be transformed
 * @param dims dimensions of rdata array
 * @param w width of Gaussian profile
 * @param init (optional) initialize without computing anything
 * @param destroy (optional) destroy fft plans
 * @param fdata
 */
void HiPassFilterC(double* rdata, int32_t* dims, double* w, bool* init, bool* destroy, double* fdata);

#ifdef __cplusplus
}
#endif



#endif /*_EMSOFTSEMWRAPPERS_H_*/
