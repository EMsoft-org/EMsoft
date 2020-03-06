#ifndef _emsoft_DIwrappers_H_
#define _emsoft_DIwrappers_H_


#ifdef __cplusplus
extern "C" {
#endif


/**
* @brief This is the typedef for a call back function that is used in the EMsoft library.
* @param size_t Unique integer that designates which C++ object did the call into EMsoft
* @param int   number of loops completed
* @param int   total number of indexing loops
* @param float time remaining in seconds (first call: total expected time)
*/
typedef void (*ProgCallBackTypeTimingdriver)(size_t, int, int, float);

/**
* @brief This is the typedef for a call back function that is used in the EMsoft library.
* @param size_t Unique integer that designates which C++ object did the call into EMsoft
* @param int   number of triplets in Euler dictionary array
* @param float pointer to array to possible euler angle triplets
* @param float pointer to the array of dot products
* @param int   pointer to the array of indices into the orientation array
*/
typedef void (*ProgCallBackTypeDIdriver)(size_t, int, float**, float**, int32_t**);

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
 * @param timingcallback callback routine to report time remaining
 * @param errorcallback callback routine to report OpenCL error code
 * @param object unique identifier for calling class instantiation
 * @param cancel boolean to trigger cancellation of computation
 */
void EBSDDIdriver(char* nmlfile, char* progname, ProgCallBackTypeDIdriver callback, ProgCallBackTypeTimingdriver timingcallback, ProgCallBackTypeErrorDIdriver errorcallback, size_t object,
                  bool* cancel);

#ifdef __cplusplus
}
#endif

#endif /*_EMSOFTDIWRAPPERS_H_*/
