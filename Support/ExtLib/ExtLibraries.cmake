# ---------- Find HDF5 Headers/Libraries -----------------------
# HDF5 now comes with everything that is needed for CMake to load
# up the targets (Exported) that it needs. We just need to find where
# HDF5 is installed.
include(${CMP_SOURCE_DIR}/ExtLib/HDF5Support.cmake)



# ---------- Find Json Fortran Headers/Libraries -----------------------
# Json-Fortran comes with everything that cmake needs to determine the
# include directories, libraries and other items. One only needs to put
# the correct path onto the CMAKE_PREFIX_PATH variable.
if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  find_package(jsonfortran-gnu REQUIRED)
  if( NOT jsonfortran-gnu_FOUND)
    message(STATUS "jsonfortran is REQUIRED for this project.")
    message(STATUS "jsonfortran source repository is at https://github.com/jacobwilliams/json-fortran")
    message(FATAL_ERROR "Please Download, Build and install. After install export the environment variable JSONFORTRAN_INSTALL to point to the installation location.")
  endif()
endif()

if (Fortran_COMPILER_NAME MATCHES "ifort.*")

  # Find specific IFort libraries.
  include(${CMP_SOURCE_DIR}/ExtLib/IFortSupport.cmake)
  
  find_package(jsonfortran-intel REQUIRED)
  if( NOT jsonfortran-intel_FOUND)
    message(STATUS "jsonfortran is REQUIRED for this project.")
    message(STATUS "jsonfortran source repository is at https://github.com/jacobwilliams/json-fortran")
    message(FATAL_ERROR "Please Download, Build and install. After install export the environment variable JSONFORTRAN_INSTALL to point to the installation location.")
  endif()
endif()

set(CMAKE_FIND_DEBUG_MODE 0)
GET_FILENAME_COMPONENT (jsonfortran_LIBRARY_DIRS "${jsonfortran_INCLUDE_DIRS}" PATH)
set(jsonfortran_LIBRARY_DIRS ${jsonfortran_LIBRARY_DIRS}/lib)
message(STATUS "jsonfortran Location: ${JSONFORTRAN_INSTALL}")
# message(STATUS "jsonfortran Version: ${jsonfortran_VERSION}")
# message(STATUS "jsonfortran LIBRARY DIR: ${jsonfortran_LIBRARY_DIRS}")
include_directories(${jsonfortran_INCLUDE_DIRS})

#------------------------------------------------------------------------------
# Find the Intel Math Kernel Library (MKL) which has FFT functions
# On mac systems, we will also need to build up the RPATH
if (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # Define the interface layers and link type for MKL
  set(MKL_Link_Type Static)
  set(MKL_Interface_Layer 32)
  set(MKL_ThreadingLayer Sequential)
  set(MKL_OpenMP_Library iomp5)
  set(MKL_F95Interface BLAS95 LAPACK95)
  find_package(MKL REQUIRED)
  if(NOT MKL_FOUND)
    message(FATAL_ERROR "MKL is Required when using the Intel Fortran Compiler")
  endif()
  set(EMsoft_OpenMP_LIBRARY ${MKL_${MKL_OpenMP_Library}_LIBRARY})
  if(EMsoft_OpenMP_LIBRARY)
    get_filename_component(EMsoft_OpenMP_LIB_DIR ${EMsoft_OpenMP_LIBRARY} DIRECTORY)
    message(STATUS "EMsoft_OpenMP_LIB_DIR: ${EMsoft_OpenMP_LIB_DIR}")
    set(EMsoft_OpenMP_LIB_DIR ${EMsoft_OpenMP_LIB_DIR} CACHE PATH "")
    get_property(EMsoftSearchDirs GLOBAL PROPERTY EMsoftSearchDirs)
    file(APPEND "${EMsoftSearchDirs}" "${EMsoft_OpenMP_LIB_DIR};")
  endif()


endif()


#------------------------------------------------------------------------------
# Find the GFotran Specific or matched libraries
if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  include(${CMP_SOURCE_DIR}/Modules/FindFFTW3.cmake)

  set(EMsoft_FORTRAN_SUPPORT_LIBS ${EMsoft_FORTRAN_SUPPORT_LIBS} gcc_eh gomp)
  # Find an OpenMP Library
  set(EMsoft_OpenMP_LIBRARY gomp)


  # Find BLAS/LAPACK Library
  if(APPLE)
    find_library(EMsoft_BLAS_LAPACK_LIBS Accelerate)
  else()
    find_package(LAPACK REQUIRED)
    set(EMsoft_BLAS_LAPACK_LIBS ${LAPACK_LIBRARIES})
  endif()
endif()

#include_directories(${JSONFORTRAN_INCLUDE_DIR} ${FFTW3_INCLUDE_DIR} ${CLFortran_INCLUDE_DIR})

#------------------------------------------------------------------------------
# Find the OpenCL Package
find_package( OpenCL REQUIRED )
if(OpenCL_FOUND)
  message(STATUS "OpenCL_FOUND: ${OpenCL_FOUND}")
  message(STATUS "OpenCL_VERSION_STRING: ${OpenCL_VERSION_STRING}")
#   message(STATUS "OpenCL_INCLUDE_DIRS: ${OpenCL_INCLUDE_DIRS}")
#   message(STATUS "OpenCL_LIBRARIES: ${OpenCL_LIBRARIES}")
#   message(STATUS "OpenCL_INCLUDE_DIR: ${OpenCL_INCLUDE_DIR}")
#   message(STATUS "OpenCL_LIBRARY: ${OpenCL_LIBRARY}")
else()
  message(FATAL_ERROR "OpenCL is needed to compile some programs in EMSoft. Please install a package appropriate for your Operating System")
endif()

include_directories( ${OpenCL_INCLUDE_DIRS} )
set(OPENCL_LIBRARY_DEBUG "${OpenCL_LIBRARY}")
set(OPENCL_LIBRARY_RELEASE "${OpenCL_LIBRARY}")

# Figure out if the OpenCL Package has CPP bindings
if( OPENCL_HAS_CPP_BINDINGS )
  message(STATUS "OpenCL has CPP bindings:: YES\n   Full include is: " ${OpenCL_INCLUDE_DIRS} )
else( OPENCL_HAS_CPP_BINDINGS )
  message(STATUS "OpenCL has CPP bindings:: NO" )
endif( OPENCL_HAS_CPP_BINDINGS )

#------------------------------------------------------------------------------
# Find the Fortran OpenCL Bindings Package
find_package(CLFortran REQUIRED)
if( NOT CLFortran_FOUND)
  message(STATUS "CLFortran is REQUIRED for this project but was not found.")
  message(STATUS "This can happen if CLFortran was built with a different GFortran.")
  message(STATUS "CLFortran_DIR: ${CLFortran_DIR}")
  message(STATUS "CMAKE_Fortran_COMPILER: ${CMAKE_Fortran_COMPILER}")
  message(STATUS "CLFortran source repository is at http://code.google.com/p/fortrancl/downloads/list")
  message(FATAL_ERROR "Please Download, Build and install. After install export the environment variable CLFortran_DIR to point to the folder that contains the CLFortranConfig.cmake file.")
else()
  get_target_property(CLFortran_LIB_PATH clfortran IMPORTED_LOCATION)
  message(STATUS "CLFortran Found: ${CLFortran_LIB_PATH}")
endif()

#----------------------------------------------------------------
# 
#
function(AddBCLSCopyInstallRules)
  set(options )
  set(oneValueArgs LIBNAME LIBVAR)
  set(multiValueArgs TYPES)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(INTER_DIR "")

  # message(STATUS "Z_LIBNAME: ${Z_LIBNAME}")
  # message(STATUS "Z_LIBVAR: ${Z_LIBVAR}")
  # message(STATUS "Z_TYPES: ${Z_TYPES}")

  set(bclsLibName ${Z_LIBNAME})

  set(Z_INSTALL_DIR "lib")
  if(WIN32)
    set(Z_INSTALL_DIR "bin")
  endif()

  FOREACH(BTYPE ${Z_TYPES} )
    # message(STATUS "  BTYPE: ${BTYPE}")
    STRING(TOUPPER ${BTYPE} TYPE)
    if(MSVC_IDE)
      set(INTER_DIR "${BTYPE}/")
    endif()

    # Get the Actual Library Path and create Install and copy rules
    GET_TARGET_PROPERTY(LibPath ${bclsLibName} IMPORTED_LOCATION_${TYPE})
    # message(STATUS "  LibPath: ${LibPath}")
    if(NOT "${LibPath}" STREQUAL "LibPath-NOTFOUND")
      if(NOT TARGET ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy)
        # message(STATUS "Creating Install And Copy Rule for ${LibPath}")
        install(FILES ${LibPath}
          DESTINATION "${Z_INSTALL_DIR}"
          CONFIGURATIONS ${BTYPE}
          COMPONENT Applications)

        if(NOT EXISTS "${LibPath}")
          message(STATUS "DOES NOT EXIST: ${LibPath}")
        endif()
        # message(STATUS "    ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}")
        ADD_CUSTOM_TARGET(ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy ALL
                          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LibPath}
                          ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}
                          COMMENT "  Copy: ${LibPath} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}"
                          )
        set_target_properties(ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/${Z_LIBVAR})

      endif()
    endif()
  endforeach()
endfunction()


#------------------------------------------------------------------------------
# Find the BCLS package (Bound-Constrained Least Squares)
find_package(bcls)
include_directories(${bcls_INCLUDE_DIRS})
if(bcls_BUILD_SHARED_LIBS STREQUAL "ON" AND WIN32)

  if(MSVC_IDE)
    set(BUILD_TYPES Debug Release)
  else()
    set(BUILD_TYPES "${CMAKE_BUILD_TYPE}")
    if("${BUILD_TYPES}" STREQUAL "")
        set(BUILD_TYPES "Debug")
    endif()
  endif()

  AddBCLSCopyInstallRules(LIBVAR bcls
                          LIBNAME bcls::bcls
                          TYPES ${BUILD_TYPES})
endif()
