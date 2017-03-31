# This is the EMsoft_SDK File. This file contains all the paths to the dependent libraries.
if(NOT DEFINED EMsoft_FIRST_CONFIGURE)
  message(STATUS "*******************************************************")
  message(STATUS "* EMsoft First Configuration Run                    *")
  message(STATUS "* EMsoft_SDK Loading from ${CMAKE_CURRENT_LIST_DIR}  *")
  message(STATUS "*******************************************************")

endif()

set(CMAKE_Fortran_FLAGS "/W1 /nologo /fpp /libs:dll /threads /assume:byterecl" CACHE STRING "" FORCE)
set(CMAKE_EXE_LINKER_FLAGS " /machine:x64 /STACK:100000000" CACHE STRING "" FORCE)

set(BUILD_TYPE ${CMAKE_BUILD_TYPE})
if("${BUILD_TYPE}" STREQUAL "")
    set(BUILD_TYPE "Release" CACHE STRING "" FORCE)
endif()
message(STATUS "BUILD_TYPE: ${BUILD_TYPE}")

set(EMsoft_SDK_ROOT "C:/EMsoft_SDK")
set(BUILD_SHARED_LIBS OFF CACHE BOOL "")
set(EMsoft_DATA_DIR ${EMsoft_SDK_ROOT}/EMsoft_Data CACHE PATH "")


#--------------------------------------------------------------------------------------------------
# Currently EMsoft does not Depend on Qt5, but if it did, this line is needed.
# Qt 5.4.x Library
# set(Qt5_DIR "${EMsoft_SDK_ROOT}/Qt5.4.2/5.4/clang_64/lib/cmake/Qt5" CACHE PATH "")

#--------------------------------------------------------------------------------------------------
# jsonfortran Library
set(JSONFORTRAN_INSTALL "${EMsoft_SDK_ROOT}/jsonfortran-intel-4.2.0")
set(JSONFORTRAN_DIR "${JSONFORTRAN_INSTALL}/cmake")
set(jsonfortran-intel_DIR "${JSONFORTRAN_DIR}")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${JSONFORTRAN_DIR} CACHE PATH "")


#-----------------------------------------------------------------------------
# Set the HDF5_INSTALL and HDF5_DIR CMake variables
set(HDF5_INSTALL "${EMsoft_SDK_ROOT}/hdf5-1.8.15" CACHE PATH "")
set(HDF5_DIR "${EMsoft_SDK_ROOT}/hdf5-1.8.15/cmake" CACHE PATH "")

#-----------------------------------------------------------------------------
# Append the HDF5 Path to the CMake Module Path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${HDF5_DIR}" CACHE PATH "")
#set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${EMsoft_SDK_ROOT}/hdf5-1.8.16/cmake") 

#--------------------------------------------------------------------------------------------------
# FORTRAN CL Library
set(CLFORTRAN_INSTALL "${EMsoft_SDK_ROOT}/CLFortran")
set(CLFORTRAN_DIR "${EMsoft_SDK_ROOT}/CLFortran/lib/CMake/CLFortran")
#--------------------------------------------------------------------------------------------------
# Update CMake Module Path with additional paths in order to better find the libraries.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CLFORTRAN_DIR}" CACHE PATH "")


#--------------------------------------------------------------------------------------------------
# Only Run this the first time when configuring DREAM.3D. After that the values
# are cached properly and the user can add additional plugins through the normal
# CMake GUI or CCMake programs.
if(NOT DEFINED EMsoft_FIRST_CONFIGURE)
  set(EMsoft_FIRST_CONFIGURE "ON" CACHE STRING "Determines if DREAM3D has already been configured")
endif()


#--------------------------------------------------------------------------------------------------
# FFTW3 Library
set(FFTW3_INSTALL "${EMsoft_SDK_ROOT}/fftw-3.3.4-dll64/fftw-3.3.4-dll64")
set(FFTW3_VERSION "3.3.4")
set(FFTW3_INCLUDE_DIR "${EMsoft_SDK_ROOT}/fftw-3.3.4-dll64")
set(FFTW3_LIBRARY "${EMsoft_SDK_ROOT}/fftw-3.3.4-dll64/libfftw3-3.lib")
set(FFTW3_IS_SHARED TRUE)

