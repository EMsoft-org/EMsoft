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
    (size_t* ipar, float* fpar, char* spar, float* mask, 
     float* exptIQ, float* ADPmap, ProgCallBackTypeDI2 callback, 
     size_t object, bool* cancel);


#ifdef __cplusplus
}
#endif



#endif /*_EMSOFTDIWRAPPERS_H_*/