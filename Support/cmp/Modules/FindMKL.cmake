# - Try to find the Intel Math Kernel Library
# Once done this will define
#
#  MKL_FOUND - system has MKL
#  MKL_ROOT_DIR - path to the MKL base directory
#  MKL_INCLUDE_DIR - the MKL include directory
#  MKL_LIBRARIES - MKL libraries
#
# There are few sets of libraries:
# Array indexes modes:
# LP - 32 bit indexes of arrays
# ILP - 64 bit indexes of arrays
# Threading:
# SEQUENTIAL - no threading
# INTEL - Intel threading library
# GNU - GNU threading library
# MPI support
# NOMPI - no MPI support
# INTEL - Intel MPI library
# OPEN - Open MPI library
# SGI - SGI MPT Library

set(MKL_ARCH_DIR "ia32")
if (${CMAKE_SIZEOF_VOID_P} EQUAL 8)
    set(MKL_ARCH_DIR "intel64")
endif()

if (FORCE_BUILD_32BITS)
    set(MKL_ARCH_DIR "ia32")
endif()

set(MKL_THREAD_VARIANTS SEQUENTIAL GNUTHREAD INTELTHREAD)
set(MKL_MODE_VARIANTS ILP LP)
set(MKL_MPI_VARIANTS NOMPI INTELMPI OPENMPI SGIMPT)

set(CMAKE_FIND_DEBUG_MODE 1)

#-------------------------------------------------------------------------------
# We only support Intel Visual Fortran 2016 and newer since those have a sane
# folder structure scheme. These next lines make sense on Windows Intel installs,
# let's hope that their installations on Linux and macOS are about the same
get_filename_component(IFORT_COMPILER_ROOT_DIR ${CMAKE_Fortran_COMPILER} DIRECTORY)
get_filename_component(IFORT_COMPILER_ROOT_DIR ${IFORT_COMPILER_ROOT_DIR} DIRECTORY)
get_filename_component(IFORT_COMPILER_ROOT_DIR ${IFORT_COMPILER_ROOT_DIR} DIRECTORY)

set(MKL_POSSIBLE_LOCATIONS
    $ENV{MKLDIR}
    /opt/intel/mkl
    /opt/intel/cmkl
    /Library/Frameworks/Intel_MKL.framework/Versions/Current/lib/universal
    "${IFORT_COMPILER_ROOT_DIR}/mkl"
    # "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2016/windows/mkl"
    # "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl"
    # "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2018/windows/mkl"
)

#-------------------------------------------------------------------------------
# Find the MKL_ROOT_DIR by finding known header files that MUST be present
find_path(MKL_ROOT_DIR 
        NAMES include/mkl_cblas.h include/blas.f90 
        PATHS ${MKL_POSSIBLE_LOCATIONS})
message(STATUS "MKL_ROOT_DIR: ${MKL_ROOT_DIR}")

IF (NOT MKL_ROOT_DIR)
  MESSAGE(WARNING "Could not find MKL: disabling it")
  set(USE_MKL FALSE)
endif()

if (USE_MKL)
    find_path(MKL_INCLUDE_DIR NAMES mkl_cblas.h blas.f90
            PATHS ${MKL_ROOT_DIR}/include ${INCLUDE_INSTALL_DIR})

    message(STATUS "MKL_INCLUDE_DIR: ${MKL_INCLUDE_DIR}")
    find_path(MKL_FFTW_INCLUDE_DIR NAMES fftw3.h fftw3.f fftw3.f03
                PATH_SUFFIXES fftw 
                PATHS ${MKL_ROOT_DIR}/include ${INCLUDE_INSTALL_DIR} 
                NO_DEFAULT_PATH)
    message(STATUS "MKL_FFTW_INCLUDE_DIR: ${MKL_FFTW_INCLUDE_DIR}")
    if (WIN32)
        set(MKL_LIB_SEARCHPATH  $ENV{ICC_LIB_DIR} 
                                $ENV{MKL_LIB_DIR} 
                                "${MKL_ROOT_DIR}/lib/${MKL_ARCH_DIR}" 
                                "${IFORT_COMPILER_ROOT_DIR}/compiler" 
                                "${IFORT_COMPILER_ROOT_DIR}/compiler/lib/${MKL_ARCH_DIR}")
        
        if(MKL_INCLUDE_DIR MATCHES "2016")
            if(CMAKE_CL_64)
                SET(MKL_LIBS mkl_intel_lp64 mkl_core mkl_intel_thread mkl_lapack95_lp64 mkl_blas95_lp64 )
            else()
                SET(MKL_LIBS mkl_intel_c mkl_core mkl_intel_thread mkl_lapack95 mkl_blas95 )
            endif()
        elseif(MKL_ROOT_DIR MATCHES "2017") # With Intel 2017 it would seem that MKL_INCLUDE_DIR does not have the year in it.
            if(CMAKE_CL_64)
                SET(MKL_LIBS mkl_intel_lp64 mkl_core mkl_intel_thread mkl_lapack95_lp64 mkl_blas95_lp64 )
            else()
                SET(MKL_LIBS mkl_intel_c mkl_core mkl_intel_thread mkl_lapack95 mkl_blas95 )
            endif()
        elseif(MKL_ROOT_DIR MATCHES "2018") # With Intel 2017 it would seem that MKL_INCLUDE_DIR does not have the year in it.
            if(CMAKE_CL_64)
                SET(MKL_LIBS mkl_intel_lp64 mkl_core mkl_intel_thread mkl_lapack95_lp64 mkl_blas95_lp64 )
            else()
                SET(MKL_LIBS mkl_intel_c mkl_core mkl_intel_thread mkl_lapack95 mkl_blas95 )
            endif()
        else() # we found a compiler in the symlinked dir which does not have a year stamp on it. Let's assume it is one of the years we support
            if(CMAKE_CL_64)
                SET(MKL_LIBS mkl_intel_lp64 mkl_core mkl_intel_thread mkl_lapack95_lp64 mkl_blas95_lp64 )
            else()
                SET(MKL_LIBS mkl_intel_c mkl_core mkl_intel_thread mkl_lapack95 mkl_blas95 )
            endif()
        endif()
        
        foreach (LIB ${MKL_LIBS})
            find_library(${LIB}_PATH ${LIB} PATHS ${MKL_LIB_SEARCHPATH} ENV LIBRARY_PATH)
            if (${LIB}_PATH)
                set(MKL_LIBRARIES ${MKL_LIBRARIES} ${${LIB}_PATH})
            else()
                message(FATAL_ERROR "Could not find ${LIB}: disabling MKL")
                BREAK()
            endif()
        endforeach()
        set(MKL_FOUND ON)

    else() # UNIX and macOS

        set(MKL_LIBRARY_LOCATIONS ${MKL_ROOT_DIR}/lib/${MKL_ARCH_DIR} ${MKL_ROOT_DIR}/lib)

        find_library(MKL_CORE_LIBRARY mkl_core PATHS ${MKL_LIBRARY_LOCATIONS})

        # Threading libraries

        find_library(MKL_RT_LIBRARY mkl_rt PATHS ${MKL_LIBRARY_LOCATIONS})
        find_library(MKL_SEQUENTIAL_LIBRARY mkl_sequential PATHS ${MKL_LIBRARY_LOCATIONS})
        find_library(MKL_INTELTHREAD_LIBRARY mkl_intel_thread PATHS ${MKL_LIBRARY_LOCATIONS})
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
            MKL_SEQUENTIAL_LIBRARY MKL_INTELTHREAD_LIBRARY MKL_GNUTHREAD_LIBRARY)
    endif()
            
    #link_directories(${MKL_ROOT_DIR}/lib/${MKL_ARCH_DIR}) # hack

    include(FindPackageHandleStandardArgs)
    find_package_handle_standard_args(MKL DEFAULT_MSG MKL_INCLUDE_DIR MKL_LIBRARIES)

#    mark_as_advanced(MKL_INCLUDE_DIR MKL_LIBRARIES)
endif()
