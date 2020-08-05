// Define callbacks for EMsoftCEBSDDI and EMsoftCRefine
#ifndef _emsoft_DIwrappers_callbacks_H_
#define _emsoft_DIwrappers_callbacks_H_
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*!
 * C-function for the callback process used by EMsoftCEBSDDI;
 * Default function for the input 'cproc';
 * Correspond to the function DIProcessTiming in
    EMsoft/Source/EMsoftWorkbench/Workbench/Modules/DictionaryIndexingModule/DictionaryIndexingController.cpp

 * 08/05/20 Currently left blank;
 */
void DIProcessTiming(size_t instance, int loopCompleted, int totalLoops, float timeRemaining);


/*!
 * OpenCL error callback process used by EMsoftCEBSDDI;
 * Default function for the input 'cerrorproc';
 * Correspond to the function DIProcessError in
    EMsoft/Source/EMsoftWorkbench/Workbench/Modules/DictionaryIndexingModule/DictionaryIndexingController.cpp

 * 08/05/20 Currently left blank;
 */
void DIProcessError(size_t instance, int nDict);


/*!
 * C-function for the callback process used by EMsoftCEBSDRefine;
 * Default function for the input 'cproc';
 * Refer to Line 1932 in EMDIwrappermod.f90:
    call proc(objAddress, globalcount, totnumexpt)

 * 08/05/20 Currently left blank;
 */
void DIProcessRefine(size_t instance, int param_a, int param_b);

#ifdef __cplusplus
}
#endif
#endif