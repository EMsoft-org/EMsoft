# - Try to find the Intel Math Kernel Library
# Once done this will define
#
#  MKL_FOUND - system has MKL
#  MKL_ROOT_DIR - path to the MKL base directory
#  MKL_INCLUDE_DIR - the MKL include directory
#  MKL_LIBRARIES - MKL libraries
#
# MKL should be found by first setting the following layers:
# MKL_Link_Type   Static|Dynamic|Single
# MKL_Interface_Layer 32|64
# MKL_ThreadingLayer  OpenMP|TBB|Sequential
# MKL_OpenMP_Library  iomp5  Only needed if MKL_ThreadingLayer = OpenMP
# MKL_F95Interface BLAS95 and/or LAPACK95
# These all need to be set correctly and then the typical find_package() can be used.
# When linking your executable or library you can use ${MKL_LIBRARIES}
#
#
set(MKL_ARCH_DIR "ia32")
if (${CMAKE_SIZEOF_VOID_P} EQUAL 8)
    set(MKL_ARCH_DIR "intel64")
    set(CMAKE_CL_64 1)
endif()

set(MKL_Link_Types Static Dynamic Single)

set(MKL_InterfaceLayers 64)
if(NOT APPLE)
set(MKL_InterfaceLayers 64 32 )
endif()

set(MKL_ThreadingLayers OpenMP TBB Sequential)
set(MKL_OpenMP_Libraries  iomp5)

set(MKL_ClusterLibraries CDFT ScaLAPACK BLACS)
set(MKL_MPILibraries MPICH)
set(MKL_F95Interfaces BLAS95 LAPACK95)

if(NOT MKL_Link_Type)
    message(FATAL_ERROR "MKL_Link_Type needs to be set to one of: ${MKL_Link_Types}")
endif()
if(NOT MKL_Interface_Layer)
    message(FATAL_ERROR "MKL_Interface_Layer needs to be set to one of: ${MKL_Interface_Layers}")
endif()
if(NOT MKL_ThreadingLayer)
    message(FATAL_ERROR "MKL_ThreadingLayer needs to be set to one of: ${MKL_ThreadingLayers}")
endif()

if("${MKL_ThreadingLayer}" STREQUAL "OpenMP")
    if(NOT MKL_OpenMP_Library)
        message(FATAL_ERROR "MKL_OpenMP_Library needs to be set to one of: ${MKL_OpenMP_Libraries}")
    endif()
endif()

if(NOT MKL_F95Interface)
    message(STATUS "MKL: Optional BLAS95 and/or LAPACK95 interfaces are not set. None, Either or Both are allowed.")
endif()


#-------------------------------------------------------------------------------
# We only support Intel Visual Fortran 2016 and newer since those have a sane
# folder structure scheme. These next lines make sense on Windows Intel installs,
# let's hope that their installations on Linux and macOS are about the same
if(NOT "${CMAKE_Fortran_COMPILER}" STREQUAL "")
    get_filename_component(IFORT_COMPILER_ROOT_DIR ${CMAKE_Fortran_COMPILER} DIRECTORY)
    get_filename_component(IFORT_COMPILER_ROOT_DIR ${IFORT_COMPILER_ROOT_DIR} DIRECTORY)
    if(WIN32)
        get_filename_component(IFORT_COMPILER_ROOT_DIR ${IFORT_COMPILER_ROOT_DIR} DIRECTORY)
    endif()
endif()


#-------------------------------------------------------------------------------
# Find the MKL_INCLUDE_DIR by finding known header files that MUST be present
set(MKL_INCLUDE_SEARCH_DIRS
    /opt/intel/mkl/include
    /opt/intel/cmkl/include
)
if(NOT "$ENV{MKLDIR}" STREQUAL "")
    set(MKL_INCLUDE_SEARCH_DIRS ${MKL_INCLUDE_SEARCH_DIRS} $ENV{MKLDIR}/include)
endif()
if(NOT "${IFORT_COMPILER_ROOT_DIR}" STREQUAL "")
    set(MKL_INCLUDE_SEARCH_DIRS ${MKL_INCLUDE_SEARCH_DIRS} "${IFORT_COMPILER_ROOT_DIR}/mkl/include")
endif()
if(NOT "${MKL_DIR}" STREQUAL "")
    set(MKL_INCLUDE_SEARCH_DIRS ${MKL_INCLUDE_SEARCH_DIRS} "${MKL_DIR}/include")
endif()
# message(STATUS "MKL_INCLUDE_SEARCH_DIRS: ${MKL_INCLUDE_SEARCH_DIRS}") 

#-------------------------------------------------------------------------------
# Find the C/C++ header version of MKL
find_path(MKL_INCLUDE_DIR 
        NAMES mkl.fi
        PATHS ${MKL_INCLUDE_SEARCH_DIRS}
)
if(NOT MKL_INCLUDE_DIR)
    message(FATAL_ERROR "mklfih was not found. Is the C/C++ version of MKL installed? Looked in:\n   ${MKL_INCLUDE_SEARCH_DIRS} ")
endif()


#-------------------------------------------------------------------------------
# Find the FFTW3_INCLUDE_DIR by finding known header files that MUST be present
set(MKL_INCLUDE_SEARCH_DIRS
    /opt/intel/mkl/include/fftw
    /opt/intel/cmkl/include/fftw
)
if(NOT "$ENV{MKLDIR}" STREQUAL "")
    set(MKL_INCLUDE_SEARCH_DIRS ${MKL_INCLUDE_SEARCH_DIRS} $ENV{MKLDIR}/include/fftw)
endif()
if(NOT "${IFORT_COMPILER_ROOT_DIR}" STREQUAL "")
    set(MKL_INCLUDE_SEARCH_DIRS ${MKL_INCLUDE_SEARCH_DIRS} "${IFORT_COMPILER_ROOT_DIR}/mkl/include/fftw")
endif()
if(NOT "${MKL_DIR}" STREQUAL "")
    set(MKL_INCLUDE_SEARCH_DIRS ${MKL_INCLUDE_SEARCH_DIRS} "${MKL_DIR}/include/fftw")
endif()
# message(STATUS "MKL_INCLUDE_SEARCH_DIRS: ${MKL_INCLUDE_SEARCH_DIRS}")      

find_path(FFTW3_INCLUDE_DIR 
        NAMES fftw3.f03
        PATHS ${MKL_INCLUDE_SEARCH_DIRS}
        )
# message(STATUS "FFTW3_INCLUDE_DIR: ${FFTW3_INCLUDE_DIR}")


#-------------------------------------------------------------------------------
# Generate the directories that will get searched for the MKL and support libs
get_filename_component(MKL_LIB_DIR ${MKL_INCLUDE_DIR} DIRECTORY)
set(MKL_LIB_DIR "${MKL_LIB_DIR}/lib")
set(MKL_LIB_DIRS "${MKL_LIB_DIR}")
if(NOT APPLE)
    set(MKL_LIB_DIRS "${MKL_LIB_DIR}/${MKL_ARCH_DIR}")
endif()
if(NOT "${IFORT_COMPILER_ROOT_DIR}" STREQUAL "")
    if(NOT APPLE)
        get_filename_component(IFORT_COMPILER_ROOT_DIR ${IFORT_COMPILER_ROOT_DIR} DIRECTORY)
    endif()
endif()
set(MKL_LIB_DIRS 
    "${MKL_LIB_DIRS}"
    "${IFORT_COMPILER_ROOT_DIR}/lib"  # MacOS and Linux(?)
    "${IFORT_COMPILER_ROOT_DIR}/redist/${MKL_ARCH_DIR}/compiler" # Windows
)


#message(STATUS "MKL_LIB_DIRS: ${MKL_LIB_DIRS}")


#-------------------------------------------------------------------------------
# Clear the MKL_LIBRARIES Variable which will hold all the libraries
set(MKL_LIBRARIES "")

if("${MKL_Link_Type}" STREQUAL "Static")
    set(MKL_LIB_PREFIX ${CMAKE_STATIC_LIBRARY_PREFIX})
    set(MKL_LIB_SUFFIX ${CMAKE_STATIC_LIBRARY_SUFFIX})
else()
    set(MKL_LIB_PREFIX ${CMAKE_SHARED_LIBRARY_PREFIX})
    set(MKL_LIB_SUFFIX ${CMAKE_SHARED_LIBRARY_SUFFIX})
endif()

if(CMAKE_FIND_DEBUG_MODE )
    message(STATUS "MKL_INCLUDE_DIR: ${MKL_INCLUDE_DIR}")
    message(STATUS "MKL_LIB_DIRS: ${MKL_LIB_DIRS}")
    message(STATUS "MKL_LIB_PREFIX: ${MKL_LIB_PREFIX}")
    message(STATUS "MKL_LIB_SUFFIX: ${MKL_LIB_SUFFIX}")
endif()

#-------------------------------------------------------------------------------
# Function FindMKLLibrary
# @param NAME The name of the library
# @param PREFIX The prefix to the library name, typically "mkl_"
# This function will attempt to find the MKL library defined by the NAME argument
# and append the library to the MKL_LIBRARIES cache variable.
function(FindMKLLibrary)
    set(options )
    set(oneValueArgs NAME PREFIX)
    set(multiValueArgs )
    cmake_parse_arguments(MKL "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
    
    string(TOLOWER ${MKL_NAME} mkl_lib_name)
    unset(mkl_${mkl_lib_name}_path CACHE)

    set(MKL_SearchName "${MKL_LIB_PREFIX}${MKL_PREFIX}${mkl_lib_name}${MKL_LIB_SUFFIX}")
    if(CMAKE_FIND_DEBUG_MODE)
        message(STATUS "MKL: Searching for ${MKL_SearchName}")
        message(STATUS "MKL_LIB_DIRS:")
        foreach(lib ${MKL_LIB_DIRS})
            message(STATUS "    ${lib}")
        endforeach(lib ${MKL_LIB_DIRS})
    endif()
    find_library(MKL_${mkl_lib_name}_LIBRARY
            NAMES ${MKL_SearchName}
            PATHS 
                ${MKL_LIB_DIRS} 
            ENV LIBRARY_PATH
    )
    set(MKL_LIBRARIES ${MKL_LIBRARIES} ${MKL_${mkl_lib_name}_LIBRARY} PARENT_SCOPE)
    
    if(CMAKE_FIND_DEBUG_MODE)
        message(STATUS "MKL_${mkl_lib_name}_LIBRARY: ${MKL_${mkl_lib_name}_LIBRARY}")
    endif()
endfunction(FindMKLLibrary)
#
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Find the MKL_Core library
FindMKLLibrary(NAME Core PREFIX mkl_)

#-------------------------------------------------------------------------------
# Find the MKL Interface Library 32|64
if(MKL_Interface_Layer EQUAL 32)
    set(mkl_interface_lib intel_lp64)
elseif(MKL_Interface_Layer EQUAL 64)
    set(mkl_interface_lib intel_ilp64)
endif()
FindMKLLibrary(NAME ${mkl_interface_lib} PREFIX mkl_)

#-------------------------------------------------------------------------------
# Find the MKL Threading Layer OpenMP|TBB|Sequential
if(MKL_ThreadingLayer STREQUAL "OpenMP")
    set(mkl_thread_lib intel_thread)
elseif(MKL_ThreadingLayer STREQUAL "TBB")
    set(mkl_thread_lib tbb_thread)
elseif(MKL_ThreadingLayer STREQUAL "Sequential")
    set(mkl_thread_lib sequential)
endif()
FindMKLLibrary(NAME ${mkl_thread_lib} PREFIX mkl_)

#-------------------------------------------------------------------------------
# Find the MKL F95 Interface  BLAS95 | LAPACK95
foreach(f95IFace ${MKL_F95Interface})
    if(MKL_Interface_Layer EQUAL 32)
        set(mkl_f95_lib ${f95IFace}_lp64)
    elseif(MKL_Interface_Layer EQUAL 64)
        set(mkl_f95_lib ${f95IFace}_ilp64)
    endif()
    FindMKLLibrary(NAME ${mkl_f95_lib} PREFIX mkl_)
endforeach(f95IFace ${MKL_F95Interface})

#-------------------------------------------------------------------------------
# Find the OMP Library. We are going to grab the dynamic version per Intel's docs
# advises against using the static version.
if(NOT WIN32)
    set(MKL_LIB_PREFIX ${CMAKE_SHARED_LIBRARY_PREFIX})
    set(MKL_LIB_SUFFIX ${CMAKE_SHARED_LIBRARY_SUFFIX})
    FindMKLLibrary(NAME ${MKL_OpenMP_Library} PREFIX "")
endif()

if(CMAKE_FIND_DEBUG_MODE )
    message(STATUS "MKL_LIBRARIES:")
    foreach(lib ${MKL_LIBRARIES})
        message(STATUS "    ${lib}")
    endforeach(lib ${MKL_LIBRARIES})
endif()

if(1)
else() # UNIX

    set(MKL_LIBRARY_LOCATIONS ${MKL_ROOT_DIR}/lib/${MKL_ARCH_DIR} ${MKL_ROOT_DIR}/lib)

    find_library(MKL_CORE_LIBRARY mkl_core PATHS ${MKL_LIBRARY_LOCATIONS})

    # Threading libraries

    find_library(MKL_RT_LIBRARY mkl_rt PATHS ${MKL_LIBRARY_LOCATIONS})
    find_library(MKL_SEQUENTIAL_LIBRARY mkl_sequential PATHS ${MKL_LIBRARY_LOCATIONS})
    find_library(MKL_TBBTHREAD_LIBRARY mkl_intel_thread PATHS ${MKL_LIBRARY_LOCATIONS})
    find_library(MKL_GNUTHREAD_LIBRARY mkl_gnu_thread PATHS ${MKL_LIBRARY_LOCATIONS})

    # Intel Libraries

    if (NOT "${MKL_ARCH_DIR}" STREQUAL "ia32")
        set(INTEL_LP_SUFFIX  "_lp64")
        set(INTEL_ILP_SUFFIX "_ilp64")
    endif()

    find_library(MKL_LP_LIBRARY mkl_intel%{INTEL_LP_SUFFIX} PATHS ${MKL_LIBRARY_LOCATIONS})
    find_library(MKL_ILP_LIBRARY mkl_intel${INTEL_ILP_SUFFIX} PATHS ${MKL_LIBRARY_LOCATIONS})

    # Lapack

    find_library(MKL_LAPACK_LIBRARY mkl_lapack PATHS ${MKL_LIBRARY_LOCATIONS})

    if (NOT MKL_LAPACK_LIBRARY)
        find_library(MKL_LAPACK_LIBRARY mkl_lapack95_lp64 PATHS ${MKL_LIBRARY_LOCATIONS})
    endif()

    # iomp5

    if (UNIX AND NOT APPLE)
        find_library(MKL_IOMP5_LIBRARY iomp5 PATHS ${MKL_ROOT_DIR}/../lib/${MKL_ARCH_DIR})
    endif()

    foreach (MODEVAR ${MKL_MODE_VARIANTS})
        foreach (THREADVAR ${MKL_THREAD_VARIANTS})
            if (MKL_CORE_LIBRARY AND MKL_${MODEVAR}_LIBRARY AND MKL_${THREADVAR}_LIBRARY)
                set(MKL_${MODEVAR}_${THREADVAR}_LIBRARIES
                    ${MKL_${MODEVAR}_LIBRARY} ${MKL_${THREADVAR}_LIBRARY} ${MKL_CORE_LIBRARY}
                    ${MKL_LAPACK_LIBRARY} ${MKL_IOMP5_LIBRARY})
                message("${MODEVAR} ${THREADVAR} ${MKL_${MODEVAR}_${THREADVAR}_LIBRARIES}") # for debug
            endif()
        endforeach()
    endforeach()

    set(MKL_LIBRARIES ${MKL_RT_LIBRARY})
    mark_as_advanced(MKL_CORE_LIBRARY MKL_LP_LIBRARY MKL_ILP_LIBRARY
        MKL_SEQUENTIAL_LIBRARY MKL_TBBTHREAD_LIBRARY MKL_GNUTHREAD_LIBRARY)
endif()
        

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MKL DEFAULT_MSG MKL_INCLUDE_DIR MKL_LIBRARIES MKL_LIB_DIRS)
find_package_handle_standard_args(FFTW3 DEFAULT_MSG FFTW3_INCLUDE_DIR)

