
if(NOT "${EMsoft_SDK}" STREQUAL "")
  include("${EMsoft_SDK}/EMsoft_SDK.cmake")
  message(STATUS "EMsoft_SDK Location: ${EMsoft_SDK}")
else()
  message(FATAL_ERROR "EMsoft_SDK location was not specified. Please pass -DEMsoft_SDK=xxxxxxxx  as a CMake variable.")
endif()

if(${CMAKE_MAJOR_VERSION} STREQUAL "3")
  # CMake 3.0 introduces VERSION variables for each Project. We do our own version numbering so we need to have
  # cmake ignore that policy
  cmake_policy(SET CMP0048 NEW)
  # CMake 3.0 introduces some OS X RPath policies
  cmake_policy(SET CMP0042 OLD)
endif()


cmake_minimum_required (VERSION 3.2.0)
project (EMsoft LANGUAGES C CXX Fortran  VERSION 3.0.0.0)

include(CMakeParseArguments)

set(EMsoft_VER_MAJOR "3")
set(EMsoft_VER_MINOR "1")
set(EMsoft_VER_PATCH "0")
set(EMsoft_TEST_DIR "EMsofttmp")

# ---------- Setup output Directories -------------------------
if(NOT DEFINED CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY
    ${EMsoft_BINARY_DIR}/Bin
    CACHE PATH
    "Single Directory for all Libraries"
    )
endif()

# --------- Setup the Executable output Directory -------------
if(NOT DEFINED CMAKE_RUNTIME_OUTPUT_DIRECTORY)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
    ${EMsoft_BINARY_DIR}/Bin
    CACHE PATH
    "Single Directory for all Executables."
    )
endif()

# --------- Setup the Executable output Directory -------------
if(NOT DEFINED CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY
    ${EMsoft_BINARY_DIR}/Bin
    CACHE PATH
    "Single Directory for all static libraries."
    )
endif()

#Enable use of Solution Folders
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Build shared libraries
OPTION(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
# changed to SHARED on 10/23/15, MDG
set(LIB_TYPE STATIC)
if(BUILD_SHARED_LIBS)
    set(LIB_TYPE SHARED)
    list(APPEND CMP_LIB_SEARCH_DIRS  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} )
    if(MSVC)
        list(APPEND CMP_LIB_SEARCH_DIRS ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Debug
                                    ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Release )
    endif()
endif(BUILD_SHARED_LIBS)


get_property(EMsoftBinaryDir GLOBAL PROPERTY EMsoftBinaryDir)
if("${EMsoftBinaryDir}" STREQUAL "")
  set(EMsoftBinaryDir "${EMsoftPublic_BINARY_DIR}")
  set_property(GLOBAL PROPERTY EMsoftBinaryDir "${EMsoftPublic_BINARY_DIR}")
endif()


if (NOT DEFINED CMP_SOURCE_DIR)
  set(CMP_SOURCE_DIR ${EMsoft_SOURCE_DIR}/Support/cmp)
endif()
include (${CMP_SOURCE_DIR}/cmpCMakeMacros.cmake )
include (${CMP_SOURCE_DIR}/cmpProject.cmake)

# --------------------------------------------------------------------
# Over ride CMake's built in module directory by prepending cmp's module
# directory first
set(CMAKE_MODULE_PATH ${EMsoft_SOURCE_DIR}/Support/CMakeModules ${CMAKE_MODULE_PATH})


# set(EXE_DEBUG_EXTENSION "_debug")
set(EXE_DEBUG_EXTENSION "")
	   
# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fopenmp -fno-sign-zero")
# set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
  set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -funroll-all-loops -fno-f2c -O3 -fno-sign-zero -std=gnu")
  set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fcheck=bounds -fno-f2c -O0 -g -fno-sign-zero -std=gnu")
  get_filename_component (Fortran_COMPILER_PATH ${CMAKE_Fortran_COMPILER} DIRECTORY)
  set(GFortran_LIB_DIR "${Fortran_COMPILER_PATH}/../lib")

elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)

  if(WIN32)
    set (CMAKE_Fortran_FLAGS_RELEASE "/Qopenmp /Qdiag-disable:11082 /Qip")
    set (CMAKE_Fortran_FLAGS_DEBUG   "/Qopenmp /Qdiag-disable:11082 /Qip")
    set (CMAKE_SHARED_LINKER_FLAGS "/ignore:4217 /ignore:4049")
  else()
    set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -qopenmp")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -qopenmp")
  endif()

else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message (STATUS "CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message (STATUS "Fortran compiler: " ${Fortran_COMPILER_NAME})
  message (STATUS "No optimized Fortran compiler flags are known.")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

# --------------------------------------------------------------------
# This should all be for macOS RPath Handling.
# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE) 

SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)


# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
ENDIF("${isSystemDir}" STREQUAL "-1")


include (${PROJECT_SOURCE_DIR}/Support/ExtLib/ExtLibraries.cmake)

# --------------------------------------------------------------------
# Generate our ReadMe and License Files
configure_file(${PROJECT_SOURCE_DIR}/License.txt
                ${PROJECT_BINARY_DIR}/License.txt )
configure_file(${PROJECT_SOURCE_DIR}/ReadMe.md
                ${PROJECT_BINARY_DIR}/ReadMe.md )
install(FILES ${PROJECT_SOURCE_DIR}/ReadMe.md ${PROJECT_SOURCE_DIR}/License.txt
        DESTINATION .
        COMPONENT Applications)

OPTION(EMsoft_ENABLE_TESTING "Compile the test programs" ON)
set(EMsoft_TESTING_DIR "${EMsoft_BINARY_DIR}/Testing")

include(${PROJECT_SOURCE_DIR}/Source/Source.cmake)

# --------------------------------------------------------------------
# Package the needed examples
# include(${PROJECT_SOURCE_DIR}/manuals/examples/SourceList.cmake)

# --------------------------------------------------------------------
# Package the needed IDL Files
include(${PROJECT_SOURCE_DIR}/IDL/SourceList.cmake)

# --------------------------------------------------------------------
# Package the needed PDF Manuals
include(${PROJECT_SOURCE_DIR}/manuals/CMakeLists.txt)

# --------------------------------------------------------------------
# Package the OpenCL files
include(${PROJECT_SOURCE_DIR}/opencl/SourceList.cmake)

# --------------------------------------------------------------------
# Package the needed resources
include(${PROJECT_SOURCE_DIR}/resources/SourceList.cmake)

# --------------------------------------------------------------------
# Package the needed template files
include(${PROJECT_SOURCE_DIR}/NamelistTemplates/SourceList.cmake)

# --------------------------------------------------------------------
# Include the CPack specific code
include(${EMsoft_SOURCE_DIR}/Support/CPack/PackageProject.cmake)



# --------------------------------------------------------------------
# This should be the last line in this file as it includes CTest for
# nightly testing.
if(EMsoft_ENABLE_TESTING)
  include(CTest)
  #include(${EMsoft_SOURCE_DIR}/Source/Test/SourceList.cmake)
  add_subdirectory(${EMsoft_SOURCE_DIR}/Source/Test ${PROJECT_BINARY_DIR}/Test)

endif()

