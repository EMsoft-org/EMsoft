#ifndef _emsoft_DIwrappers_H_
#define _emsoft_DIwrappers_H_


#ifdef __cplusplus
extern "C" {
#endif

/**
* @brief This is the typedef for a call back function that is used in the EMsoft library.
* @param size_t Unique integer that designates which C++ object did the call into EMsoft
* @param int
* @param int
*/
typedef void (*ProgCallBackTypeDI2)(size_t, int, int);

/**
* @brief This is the typedef for a call back function that is used in the EMsoft library.
* @param size_t Unique integer that designates which C++ object did the call into EMsoft
* @param int
* @param int
* @param float
*/
typedef void (*ProgCallBackTypeDI3)(size_t, int, int, float);

/**
* @brief This is the typedef for an OpenCL error call back function that is used in the EMsoft library.
* @param size_t Unique integer that designates which C++ object did the call into EMsoft
* @param int
*/
typedef void (*ProgCallBackTypeError)(size_t, int);

/**
* EBSD experimental pattern preprocessing:
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param spar array with string input parameters
* @param mask mask array
* @param exptIQ Image Quality array
* @param ADPmap average dot product map array
* @param callback callback routine to update progress bar
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/
void EMsoftCpreprocessEBSDPatterns
    (int32_t* ipar, float* fpar, char* spar, float* mask,
     float* exptIQ, float* ADPmap, ProgCallBackTypeDI2 callback, 
     size_t object, bool* cancel);

/**
* EBSD single experimental pattern preprocessing:
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param inputpattern  input EBSD pattern as float array
* @param outputpattern  input EBSD pattern as float array
*/
void EMsoftCpreprocessSingleEBSDPattern
    (size_t* ipar, float* fpar, float* inputpattern, float* outputpattern);

/**
* EBSD pattern preprocessing parameter range:
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param spar array with string input parameters
* @param averagedpattern  input EBSD pattern as float array
* @param patternarray  input EBSD pattern as float array
*/
void EMsoftCEBSDDIpreview
    (size_t* ipar, float* fpar, char* spar, float* averagedpattern, float* patternarray);

/**
* EBSD Dictionary indexing (all in ram) wrapper routine
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param spar array with string input parameters
* @param dpatterns array with pre-processed dictionary patterns
* @param epatterns array with pre-processed experimental patterns
* @param resultmain array with top N dot product values
* @param indexmain array with euler angle indices for top N dot product values
* @param callback callback routine to update progress bar
* @param errorcallback callback routine to report OpenCL error code
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/

void EMsoftCEBSDDI
	(int32_t* ipar, float* fpar, char* spar, float* dpatterns, float* epatterns, 
	 float* resultmain, int32_t* indexmain, ProgCallBackTypeDI3 callback, ProgCallBackTypeError errorcallback,
     size_t object, bool* cancel); 

/**
* EBSD indexing refinement (all in ram) wrapper routine
* @param ipar array with integer input parameters
* @param fpar array with float input parameters
* @param accum_e array with Monte Carlo histogram
* @param mLPNH Northern hemisphere master pattern
* @param mLPSH Southern hemisphere master pattern
* @param variants array with quaternions defining the potential pseudosymmetry variants
* @param epatterns array with pre-processed experimental patterns
* @param startEulers array with initial Euler angle triplets
* @param startdps array with initial dot product values
* @param eumain array with refined Euler angle triplets
* @param dpmain array with refined dot product values
* @param callback callback routine to update progress bar
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/

void EMsoftCEBSDRefine
	(size_t* ipar, float* fpar, int32_t* accum_e, float* mLPNH, float* mLPSH,
	 float* variants, float* epatterns, float* startEulers, float* startdps, float* eumain, 
	 float* dpmain, ProgCallBackTypeDI2 callback, size_t object, bool* cancel);


#ifdef __cplusplus
}
#endif

#endif /*_EMSOFTDIWRAPPERS_H_*/