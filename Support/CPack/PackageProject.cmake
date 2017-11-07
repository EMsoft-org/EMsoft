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
        DESTINATION ./bin
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


# Get a shorter version number:
set(EMsoft_VERSION_SHORT "${EMsoft_VER_MAJOR}.${EMsoft_VER_MINOR}")
message(STATUS "EMsoft_RELEASE_TYPE: ${EMsoft_RELEASE_TYPE}")
if("${EMsoft_RELEASE_TYPE}" STREQUAL "Official")
  set(EMsoft_VERSION_SHORT "${EMsoft_VERSION_MAJOR}.${EMsoft_VERSION_MINOR}.${EMsoft_VERSION_PATCH}")
elseif("${EMsoft_RELEASE_TYPE}" STREQUAL "Beta")
  set(EMsoft_VERSION_SHORT "${EMsoft_VERSION_MAJOR}.${EMsoft_VERSION_MINOR}-${EMsoft_RELEASE_TYPE}-${EMsoft_VERSION_TWEAK}")
elseif("${EMsoft_RELEASE_TYPE}" STREQUAL "Development")
  set(EMsoft_VERSION_SHORT "${EMsoft_VERSION_MAJOR}.${EMsoft_VERSION_MINOR}.${EMsoft_VERSION_PATCH}.${EMsoft_VERSION_TWEAK}")
else()
  set(EMsoft_VERSION_SHORT "0.0.0")
endif()


message(STATUS "EMsoft_VERSION_SHORT: ${EMsoft_VERSION_SHORT}")


SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "EMsoft Tools")
SET(CPACK_PACKAGE_VENDOR "Dr. Marc De Graef")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_BINARY_DIR}/ReadMe.md")
SET(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_BINARY_DIR}/License.txt")
SET(CPACK_PACKAGE_VERSION_MAJOR ${EMsoft_VER_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${EMsoft_VER_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${EMsoft_VER_PATCH})
SET(CPACK_PACKAGE_VERSION ${EMsoft_VERSION})
#SET(CPACK_COMPONENTS_ALL Applications Headers)
#set(CPACK_COMPONENT_APPLICATIONS_DISPLAY_NAME "Applications")
#set(CPACK_COMPONENT_APPLICATIONS_DESCRIPTION  "The EMsoft Software Tools Suite")
#set(CPACK_COMPONENT_APPLICATIONS_REQUIRED 1)
#set(CPACK_COMPONENT_HEADERS_DISPLAY_NAME "Headers")
#set(CPACK_COMPONENT_HEADERS_DESCRIPTION  "Development Header Files")
#set(CPACK_COMPONENT_HEADERS_REQUIRED 0)
#set(CPACK_COMPONENT_RUNTIME_DISPLAY_NAME "Runtime")
#set(CPACK_COMPONENT_RUNTIME_DESCRIPTION  "Runtime Libraries")
#set(CPACK_COMPONENT_RUNTIME_REQUIRED 1)

#set(CPACK_PACKAGE_EXECUTABLES  )
set(UPLOAD_FILE_NAME "")

if(APPLE)
    set(CPACK_PACKAGE_FILE_NAME "EMsoft-${EMsoft_VERSION_SHORT}-OSX")
    # This ASSUMES we are creating a tar.gz package. If you change that below to
    # anything else then you need to update this.
    set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.tar.gz)
elseif(WIN32)
    if( "${CMAKE_SIZEOF_VOID_P}" EQUAL "8" )
            set(CPACK_PACKAGE_FILE_NAME "EMsoft-${EMsoft_VERSION_SHORT}-Win64")
            set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.zip)
    elseif( "${CMAKE_SIZEOF_VOID_P}" EQUAL "4" )
            set(CPACK_PACKAGE_FILE_NAME "EMsoft-${EMsoft_VERSION_SHORT}-Win32")
            set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.zip)
    else()
        set(CPACK_PACKAGE_FILE_NAME "EMsoft-${EMsoft_VERSION_SHORT}-Unknown")
        set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.zip)
    endif()
else()
    if( "${CMAKE_SIZEOF_VOID_P}" EQUAL "8" )
            set(CPACK_PACKAGE_FILE_NAME "EMsoft-${EMsoft_VERSION_SHORT}-${EMsoft_LINUX_SYSTEM}-x86_64")
            set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.tar.gz)
    elseif( "${CMAKE_SIZEOF_VOID_P}" EQUAL "4" )
            set(CPACK_PACKAGE_FILE_NAME "EMsoft-${EMsoft_VERSION_SHORT}-${EMsoft_LINUX_SYSTEM}-i386")
            set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.tar.gz)
    else()
        set(CPACK_PACKAGE_FILE_NAME "EMsoft-${EMsoft_VERSION_SHORT}-${CMAKE_SYSTEM_NAME}")
        set(UPLOAD_FILE_NAME ${CPACK_PACKAGE_FILE_NAME}.tar.gz)
    endif()
endif()


#set(EMsoft_WEBSITE_SERVER "EMsoft.bluequartz.net")
#set(EMsoft_WEBSITE_SERVER_PATH "/var/www/EMsoft.bluequartz.net")
#set(EMsoft_WEBSITE_SCP_USERNAME "mjackson")
#-- Create a bash script file that will upload the latest version to the web server
if(WIN32)
  message(STATUS "${PROJECT_SOURCE_DIR}/Support/copy_nightly.bat.in")
  message(STATUS "${PROJECT_BINARY_DIR}/copy_nightly.bat")
    configure_file(${PROJECT_SOURCE_DIR}/Support/copy_nightly.bat.in
                  ${EMsoft_BINARY_DIR}/copy_nightly.bat)
endif()

# Create an NSIS based installer for Windows Systems
if(WIN32 AND NOT UNIX)
  # There is a bug in NSIS that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  SET(CPACK_NSIS_DISPLAY_NAME "EMsoft Software Tools")
  SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\EMsoft")
  SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\EMsoft")
  SET(CPACK_NSIS_CONTACT "EMsoft")
  SET(CPACK_NSIS_MODIFY_PATH ON)
  SET(CPACK_GENERATOR "ZIP")
  SET(CPACK_BINARY_ZIP "ON")
  SET(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "EMsoft Software Tools")
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
SET(CPACK_SOURCE_PACKAGE_FILE_NAME "EMsoft-${EMsoft_VERSION_SHORT}-Source")

#-- Create a bash script file that will upload the latest version to the web server
set(UPLOAD_FILE_NAME ${CPACK_SOURCE_PACKAGE_FILE_NAME}.tar.gz)
#configure_file(${PROJECT_RESOURCES_DIR}/upload.sh.in
#               ${PROJECT_BINARY_DIR}/src_upload.sh)

#-- Create a bash script file that will upload the latest version to the web server
#configure_file(${PROJECT_RESOURCES_DIR}/version_upload.sh.in
#               ${PROJECT_BINARY_DIR}/version_upload.sh)

SET(CPACK_SOURCE_TOPLEVEL_TAG "Source")
SET(CPACK_IGNORE_FILES "/i386/;/x64/;/VS2008/;/zRel/;/Build/;/\\\\.git/;\\\\.*project")
SET(CPACK_SOURCE_IGNORE_FILES "/i386/;/x64/;/VS2008/;/zRel/;/Build/;/\\\\.git/;\\\\.*project;\\\\.user")



# THIS MUST BE THE LAST LINE OF THIS FILE BECAUSE ALL THE CPACK VARIABLES MUST BE
# DEFINED BEFORE CPack IS INCLUDED
INCLUDE(CPack)

