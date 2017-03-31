#///////////////////////////////////////////////////////////////////////////////
#//
#//  Copyright (c) 2009, Michael A. Jackson. BlueQuartz Software
#//  All rights reserved.
#//  BSD License: http://www.opensource.org/licenses/bsd-license.html
#//
#///////////////////////////////////////////////////////////////////////////////


# ------------------------------------------------------------------------------
# This CMake code sets up for CPack to be used to generate native installers
# ------------------------------------------------------------------------------
if(MSVC)
    # Skip the install rules, we only want to gather a list of the system libraries
    SET(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP 1)
    #SET(CMAKE_INSTALL_DEBUG_LIBRARIES OFF)

    # Gather the list of system level runtime libraries
    INCLUDE (InstallRequiredSystemLibraries)

    # Our own Install rule for Release builds of the MSVC runtime libs
    if(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS)
      INSTALL(FILES ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
        DESTINATION ./
        PERMISSIONS OWNER_WRITE OWNER_READ OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ
        COMPONENT Applications
        CONFIGURATIONS Release)
    endif(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS)
endif()

if(UNIX AND NOT APPLE)
    SET(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP 0)
    if(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS)
      INSTALL(FILES ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
        DESTINATION ./lib
        PERMISSIONS OWNER_WRITE OWNER_READ OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ
        COMPONENT Applications
        CONFIGURATIONS Release)
    endif(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS)
endif()

# Add a short ReadMe file for OS X that warns of moving the applications
if(APPLE)
    install(FILES ${PROJECT_RESOURCES_DIR}/CPack/OS_X_ReadMe.txt DESTINATION .)
endif()

message(STATUS "EMsoftWorkbenchProj_RELEASE_TYPE: ${EMsoftWorkbenchProj_RELEASE_TYPE}")
if("${EMsoftWorkbenchProj_RELEASE_TYPE}" STREQUAL "Official")
  set(EMsoftWorkbench_VERSION_SHORT "${EMsoftWorkbenchProj_VERSION_MAJOR}.${EMsoftWorkbenchProj_VERSION_MINOR}.${EMsoftWorkbenchProj_VERSION_PATCH}")
elseif("${EMsoftWorkbenchProj_RELEASE_TYPE}" STREQUAL "Beta")
  set(EMsoftWorkbench_VERSION_SHORT "${EMsoftWorkbenchProj_VERSION_MAJOR}.${EMsoftWorkbenchProj_VERSION_MINOR}-${EMsoftWorkbenchProj_RELEASE_TYPE}-${EMsoftWorkbenchProj_VERSION_TWEAK}")
elseif("${EMsoftWorkbenchProj_RELEASE_TYPE}" STREQUAL "Development")
  set(EMsoftWorkbench_VERSION_SHORT "${EMsoftWorkbenchProj_VERSION_MAJOR}.${EMsoftWorkbenchProj_VERSION_MINOR}.${EMsoftWorkbenchProj_VERSION_PATCH}.${EMsoftWorkbenchProj_VERSION_TWEAK}")
else()
  set(EMsoftWorkbench_VERSION_SHORT "0.0.0")
endif()


message(STATUS "EMsoftWorkbench_VERSION_SHORT: ${EMsoftWorkbench_VERSION_SHORT}")


SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "CDAF Tools")
SET(CPACK_PACKAGE_VENDOR "BlueQuartz Software, LLC")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_BINARY_DIR}/ReadMe.md")
#SET(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_RESOURCES_DIR}/")
SET(CPACK_PACKAGE_VERSION_MAJOR ${EMsoftWorkbench_VER_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${EMsoftWorkbench_VER_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${EMsoftWorkbench_VER_PATCH})
SET(CPACK_PACKAGE_VERSION ${EMsoftWorkbench_VERSION})


set(CPACK_PACKAGE_EXECUTABLES
    EMsoftWorkbench EMsoftWorkbench StatsGenerator StatsGenerator DevHelper DevHelper)
set(UPLOAD_FILE_NAME "")

if(APPLE)
    set(CPACK_PACKAGE_FILE_NAME "EMsoftWorkbench-${EMsoftWorkbench_VERSION_SHORT}-OSX")
    # This ASSUMES we are creating a tar.gz package. If you change that below to
    # anything else then you need to update this.
    set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.tar.gz)
elseif(WIN32)
    if( "${CMAKE_SIZEOF_VOID_P}" EQUAL "8" )
            set(CPACK_PACKAGE_FILE_NAME "EMsoftWorkbench-${EMsoftWorkbench_VERSION_SHORT}-Win64")
            set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.zip)
    elseif( "${CMAKE_SIZEOF_VOID_P}" EQUAL "4" )
            set(CPACK_PACKAGE_FILE_NAME "EMsoftWorkbench-${EMsoftWorkbench_VERSION_SHORT}-Win32")
            set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.zip)
    else()
        set(CPACK_PACKAGE_FILE_NAME "EMsoftWorkbench-${EMsoftWorkbench_VERSION_SHORT}-Unknown")
        set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.zip)
    endif()
else()
    if( "${CMAKE_SIZEOF_VOID_P}" EQUAL "8" )
            set(CPACK_PACKAGE_FILE_NAME "EMsoftWorkbench-${EMsoftWorkbench_VERSION_SHORT}-${EMsoftWorkbench_LINUX_SYSTEM}-x86_64")
            set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.tar.gz)
    elseif( "${CMAKE_SIZEOF_VOID_P}" EQUAL "4" )
            set(CPACK_PACKAGE_FILE_NAME "EMsoftWorkbench-${EMsoftWorkbench_VERSION_SHORT}-${EMsoftWorkbench_LINUX_SYSTEM}-i386")
            set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.tar.gz)
    else()
        set(CPACK_PACKAGE_FILE_NAME "EMsoftWorkbench-${EMsoftWorkbench_VERSION_SHORT}-${CMAKE_SYSTEM_NAME}")
        set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.tar.gz)
    endif()
endif()

# Create an ZIP based installer for Windows Systems
if(WIN32 AND NOT UNIX)
  # There is a bug in NSIS that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  SET(CPACK_NSIS_DISPLAY_NAME "CDAF Software Tools")
  SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\www.bluequartz.net")
  SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\www.bluequartz.net")
  SET(CPACK_NSIS_CONTACT "cdaf@bluequartz.net")
  SET(CPACK_NSIS_MODIFY_PATH ON)
  SET(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "CDAF Software Tools")
ENDif(WIN32 AND NOT UNIX)

if(NOT CPACK_GENERATOR)
  if(UNIX)
    if(CYGWIN)
      option(CPACK_BINARY_CYGWIN "Enable to build Cygwin binary packages" ON)
    else(CYGWIN)
      if(APPLE)
        option(CPACK_BINARY_PACKAGEMAKER "Enable to build PackageMaker packages" OFF)
        option(CPACK_BINARY_OSXX11       "Enable to build OSX X11 packages"      OFF)
      else(APPLE)
        option(CPACK_BINARY_TZ  "Enable to build TZ packages"     OFF)
      endif(APPLE)
      option(CPACK_BINARY_STGZ "Enable to build STGZ packages"    OFF)
      option(CPACK_BINARY_TGZ  "Enable to build TGZ packages"     ON)
      option(CPACK_BINARY_TBZ2 "Enable to build TBZ2 packages"    OFF)
      option(CPACK_BINARY_DEB  "Enable to build Debian packages"  OFF)
      option(CPACK_BINARY_RPM  "Enable to build RPM packages"     OFF)
      option(CPACK_BINARY_NSIS "Enable to build NSIS packages"    OFF)
    endif(CYGWIN)
  else(UNIX)
    option(CPACK_BINARY_NSIS "Enable to build NSIS packages" OFF)
    option(CPACK_BINARY_ZIP  "Enable to build ZIP packages" ON)
  endif(UNIX)

endif(NOT CPACK_GENERATOR)


SET(CPACK_SOURCE_GENERATOR "TGZ")
SET(CPACK_SOURCE_PACKAGE_FILE_NAME "CDAF-${EMsoftWorkbench_VERSION_SHORT}-Source")


SET(CPACK_SOURCE_TOPLEVEL_TAG "Source")
SET(CPACK_IGNORE_FILES "/i386/;/x64/;/VS2008/;/zRel/;/Build/;/\\\\.git/;\\\\.*project")
SET(CPACK_SOURCE_IGNORE_FILES "/i386/;/x64/;/VS2008/;/zRel/;/Build/;/\\\\.git/;\\\\.*project")



# THIS MUST BE THE LAST LINE OF THIS FILE BECAUSE ALL THE CPACK VARIABLES MUST BE
# DEFINED BEFORE CPack IS INCLUDED
INCLUDE(CPack)

