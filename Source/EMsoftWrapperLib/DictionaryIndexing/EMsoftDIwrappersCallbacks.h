// Define callbacks for EMsoftCEBSDDI
#ifndef _emsoft_DIwrappers_callbacks_H_
#define _emsoft_DIwrappers_callbacks_H_
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif
void DIProcessTiming(size_t instance, int loopCompleted, int totalLoops, float timeRemaining);

void DIProcessError(size_t instance, int nDict);

void DIProcessRefine(size_t instance, int param_a, int param_b);

#ifdef __cplusplus
}
#endif
#endif