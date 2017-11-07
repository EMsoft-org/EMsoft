
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

set(EMsoft_VER_MAJOR "4")
set(EMsoft_VER_MINOR "0")
set(EMsoft_VER_PATCH "0")

cmake_minimum_required (VERSION 3.7.0)
project (EMsoft LANGUAGES C CXX Fortran  VERSION ${EMsoft_VER_MAJOR}.${EMsoft_VER_MINOR}.${EMsoft_VER_PATCH}.0)

include(CMakeParseArguments)


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
if(WIN32)
  set(BUILD_SHARED_LIBS ON CACHE BOOL "" FORCE)
endif()
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

get_property(EMsoftSearchDirs GLOBAL PROPERTY EMsoftSearchDirs)
if("${EMsoftSearchDirs}" STREQUAL "")
  set_property(GLOBAL PROPERTY EMsoftSearchDirs "${EMsoft_BINARY_DIR}/LibSearchDirs.txt")
  get_property(EMsoftSearchDirs GLOBAL PROPERTY EMsoftSearchDirs)
  file(WRITE "${EMsoftSearchDirs}" "${CMAKE_LIBRARY_OUTPUT_DIRECTORY};")
  file(APPEND "${EMsoftSearchDirs}" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY};")
  if(MSVC)
    file(APPEND "${EMsoftSearchDirs}"  "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Debug;${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Release;")
  endif()

endif()


if (NOT DEFINED CMP_SOURCE_DIR)
  set(CMP_SOURCE_DIR ${EMsoft_SOURCE_DIR}/Support/cmp)
endif()

# Set the Application Name to with the correct icons and badges
set(CMP_HEADER_DIR ${EMsoft_BINARY_DIR})
set(PROJECT_RESOURCES_DIR "${EMsoft_SOURCE_DIR}/resources")

set(EMsoft_APPLICATION_NAME "EMsoft")
set(EMsoft_VERSION_HDR_FILE "EMsoftVersion.h")
set(EMsoft_VERSION_SRC_FILE "EMsoftVersion.cpp")
set(EXE_DEBUG_EXTENSION "_debug")

include(${CMP_SOURCE_DIR}/cmpProject.cmake)
include (${CMP_SOURCE_DIR}/cmpCMakeMacros.cmake )

#------------------------------------------------------------------------------
# There are several different Release Types that we are going to use.
# Official: This is an official release from BlueQuartz Software
# Beta: This is a Beta release that has is getting ready to be officially released.
# Development: This is currently in development and probably has bugs.
get_property(EMsoft_RELEASE_TYPE GLOBAL PROPERTY PACKAGE_RELEASE_TYPE)
if("${EMsoft_RELEASE_TYPE}" STREQUAL "")
  set(EMsoft_RELEASE_TYPE "Development")
  set_property(GLOBAL PROPERTY PACKAGE_RELEASE_TYPE "${EMsoft_RELEASE_TYPE}")
endif()

#------------------------------------------------------------------------------
# Find the Git Package for Versioning. It should be ok if Git is NOT found
Find_package(Git)
# -----------------------------------------------------------------------
# Generate a set of Version Cpp/H files that will be used by SIMPLView to
# get and display the proper version numbers.
cmpRevisionString( GENERATED_HEADER_FILE_PATH "${EMsoft_VERSION_HDR_FILE}"
                        GENERATED_SOURCE_FILE_PATH "${EMsoft_VERSION_SRC_FILE}"
                        NAMESPACE "EMsoft"
                        PROJECT_NAME "${PROJECT_NAME}"
                        EXPORT_MACRO "EMsoft_EXPORT")

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

OPTION(EMsoft_ENABLE_TESTING "Compile the test programs" ON)
set(EMsoft_TESTING_DIR "${EMsoft_BINARY_DIR}/Testing")



# -----------------------------------------------------------------------
# Set a Global Prefix where everthing is going to get intalled. For OS X we are
# going to try to install everything into the .app package
# -----------------------------------------------------------------------
set_property(GLOBAL PROPERTY EMsoft_PACKAGE_DEST_PREFIX ".")

include(${PROJECT_SOURCE_DIR}/Source/Source.cmake)

# --------------------------------------------------------------------
# Package the needed examples
include(${PROJECT_SOURCE_DIR}/manuals/examples/SourceList.cmake)

# # --------------------------------------------------------------------
# # Package the needed IDL Files
# if(APPLE)
#   include(${PROJECT_SOURCE_DIR}/IDL/SourceList.cmake)
# endif()

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
# Generate our ReadMe and License Files
configure_file(${PROJECT_SOURCE_DIR}/License.txt
                ${PROJECT_BINARY_DIR}/License.txt )
configure_file(${PROJECT_SOURCE_DIR}/ReadMe.md
                ${PROJECT_BINARY_DIR}/ReadMe.md )

#---------------------------------------------------------------------
# This sets up the two variables install_dir and lib_install_dir
EMsoft_SetupInstallDirs()

install(FILES ${PROJECT_SOURCE_DIR}/ReadMe.md ${PROJECT_SOURCE_DIR}/License.txt
        DESTINATION ${install_dir}/Documentation
        COMPONENT Applications)

# --------------------------------------------------------------------
# Include the CPack specific code
include(${EMsoft_SOURCE_DIR}/Support/CPack/PackageProject.cmake)

# --------------------------------------------------------------------
# This should be the last line in this file as it includes CTest for
# nightly testing.
if(EMsoft_ENABLE_TESTING)
  ENABLE_TESTING()
  include(CTest)
  add_subdirectory(${EMsoft_SOURCE_DIR}/Source/Test ${PROJECT_BINARY_DIR}/Test)
endif()

