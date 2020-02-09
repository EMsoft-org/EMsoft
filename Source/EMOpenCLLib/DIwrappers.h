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
* @param float
*/
typedef void (*ProgCallBackTypeDIdriver)(size_t, int, int, float, float, int);

/**
* @brief This is the typedef for an OpenCL error call back function that is used in the EMsoft library.
* @param size_t Unique integer that designates which C++ object did the call into EMsoft
* @param int
*/
typedef void (*ProgCallBackTypeErrorDIdriver)(size_t, int);

/**
* EBSD dictionary indexing wrapper:
* @param nmlfile full path to the EMEBSDDI name list file 
* @param progname program name 
* @param dparray array of current highest dot product values
* @param indexarray array of indices into the Euler array for the current best match
* @param callback callback routine to update progress bar
* @param errorcallback callback routine to report OpenCL error code
* @param object unique identifier for calling class instantiation
* @param cancel boolean to trigger cancellation of computation
*/
void EBSDDIdriver
    (char* nmlfile, char* progname, 
     ProgCallBackTypeDIdriver callback, 
     ProgCallBackTypeErrorDIdriver errorcallback,
     size_t object, bool* cancel);


// void EBSDDIdriver
//     (int32_t* ipar, float* fpar, char* spar, float* mask,
//      float* exptIQ, float* ADPmap, ProgCallBackTypeDI2 callback, 
//      size_t object, bool* cancel);



#ifdef __cplusplus
}
#endif

#endif /*_EMSOFTDIWRAPPERS_H_*/