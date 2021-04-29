#if defined (_MSC_VER)
#include <windows.h>
#else
#include <sys/time.h>
#include <unistd.h>
#endif


#ifdef __cplusplus
extern "C" {
#endif
/**
* @brief Sleep the current thread for specific number of milliseconds
* @param millis The number of milliseconds to sleep the thread.
*/
void msleep(int millis);

#ifdef __cplusplus
}
#endif

#if defined (_MSC_VER)
void msleep(int millis)
{
  Sleep(millis);
}

#else

void msleep(int millis)
{
  // This call actually takes a microsecond argument so make the adjustment
  usleep(millis * 1000);
}
#endif
