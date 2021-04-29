#if defined (_MSC_VER)
#include <winsock2.h>
#else
#include <sys/time.h>
#endif
#include <stddef.h>

void msleep(int tms)
{
    struct timeval tv;
    tv.tv_sec  = tms / 1000;
    tv.tv_usec = (tms % 1000) * 1000;
    select (0, NULL, NULL, NULL, &tv);
}