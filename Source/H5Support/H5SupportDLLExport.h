#pragma once


#if defined (_MSC_VER)
#pragma warning(disable: 4251)
#pragma warning(disable: 4710)
#pragma warning(disable: 4820)
#pragma warning(disable: 4668)
#pragma warning(disable: 4265)
#pragma warning(disable: 4189)
#pragma warning(disable: 4640)
#pragma warning(disable: 4996)
#pragma warning(disable: 4548)
#endif

/* Cmake will define H5Support_EXPORTS on Windows when it
configures to build a shared library. If you are going to use
another build system on windows or create the visual studio
projects by hand you need to define H5Support_EXPORTS when
building the H5SupportDatModel DLL on windows.
*/

#if defined (H5Support_BUILT_AS_DYNAMIC_LIB)

#if defined (H5Support_EXPORTS)  /* Compiling the H5Support DLL/Dylib */
#if defined (_MSC_VER)  /* MSVC Compiler Case */
#define  H5Support_EXPORT __declspec(dllexport)
#elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
#define H5Support_EXPORT __attribute__ ((visibility("default")))
#endif
#else  /* Importing the DLL into another project */
#if defined (_MSC_VER)  /* MSVC Compiler Case */
#define  H5Support_EXPORT __declspec(dllimport)
#elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
#define H5Support_EXPORT __attribute__ ((visibility("default")))
#endif
#endif
#endif

/* If H5Support_EXPORT was never defined, define it here */
#ifndef H5Support_EXPORT
#define H5Support_EXPORT
#endif

#if 0
#if defined (_WIN32) || defined __CYGWIN__

#if defined (H5Support_BUILT_AS_DYNAMIC_LIB)
#if defined(H5Support_EXPORTS)
#define  H5Support_EXPORT __declspec(dllexport)
#else
#define  H5Support_EXPORT __declspec(dllimport)
#endif /* H5Support_EXPORTS */

#else
#define H5Support_EXPORT
#endif
#elif __GNUC__ >= 4
#define FLOW_DLL __attribute__ ((visibility("default")))
#define DLL_LOCAL  __attribute__ ((visibility("hidden")
#else /* defined (_WIN32) && defined (H5Support_BUILD_SHARED_LIBS)  */
#define H5Support_EXPORT
#endif
#endif


