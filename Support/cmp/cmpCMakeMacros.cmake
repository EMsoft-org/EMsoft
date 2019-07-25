#--////////////////////////////////////////////////////////////////////////////
# Copyright (c) 2009-2015 BlueQuartz Software, LLC
#
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright notice, this
# list of conditions and the following disclaimer in the documentation and/or
# other materials provided with the distribution.
#
# Neither the name of BlueQuartz Software, the US Air Force, nor the names of its
# contributors may be used to endorse or promote products derived from this software
# without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
# USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The code contained herein was partially funded by the followig contracts:
#    United States Air Force Prime Contract FA8650-07-D-5800
#    United States Air Force Prime Contract FA8650-10-D-5210
#    United States Prime Contract Navy N00173-07-C-2068
#--////////////////////////////////////////////////////////////////////////////
include (CMakeParseArguments)
# include(${CMP_OSX_TOOLS_SOURCE_DIR}/OSX_BundleTools.cmake)
# include(${CMP_OSX_TOOLS_SOURCE_DIR}/ToolUtilities.cmake)

#-------------------------------------------------------------------------------
macro (cmp_IDE_GENERATED_PROPERTIES SOURCE_PATH HEADERS SOURCES)
    STRING(REPLACE "/" "\\\\" source_group_path ${SOURCE_PATH}  )
    source_group(${source_group_path} FILES ${HEADERS} ${SOURCES})

  #-- The following is needed if we ever start to use OS X Frameworks but only
  #--  works on CMake 2.6 and greater
  #set_property(SOURCE ${HEADERS}
  #             PROPERTY MACOSX_PACKAGE_LOCATION Headers/${NAME}
  #)

endmacro (cmp_IDE_GENERATED_PROPERTIES SOURCE_PATH HEADERS SOURCES)

#-------------------------------------------------------------------------------

macro (cmp_IDE_SOURCE_PROPERTIES SOURCE_PATH HEADERS SOURCES INSTALL_FILES)
  if(${INSTALL_FILES} EQUAL "1")
    INSTALL (FILES ${HEADERS}
             DESTINATION "include/${PROJECT_NAME}/${SOURCE_PATH}"
             COMPONENT Headers
    )
  endif()
  STRING(REPLACE "/" "\\\\" source_group_path "${SOURCE_PATH}"  )
  source_group("${source_group_path}" FILES ${HEADERS} ${SOURCES})

  #-- The following is needed if we ever start to use OS X Frameworks but only
  #--  works on CMake 2.6 and greater
  #set_property(SOURCE ${HEADERS}
  #             PROPERTY MACOSX_PACKAGE_LOCATION Headers/${NAME}
  #)

endmacro (cmp_IDE_SOURCE_PROPERTIES NAME HEADERS SOURCES INSTALL_FILES)

#-------------------------------------------------------------------------------
# This macro will set all the variables necessary to have a "good" OS X Application
# bundle. The variables are as follows:
#  TARGET_NAME - which can be taken from the ${TARGET_NAME} variable is needed
#  DEBUG_EXTENSION - The extension used to denote a debug built Application. Typically
#   this is '_debug'
#  ICON_FILE_PATH - The complete path to the bundle icon file
#  VERSION_STRING - The version string that you wish to use for the bundle. For OS X
#   this string is usually XXXX.YY.ZZ in type. Look at the Apple docs for more info
#-------------------------------------------------------------------------------
macro(ConfigureMacOSXBundlePlist TARGET_NAME DEBUG_EXTENSION ICON_FILE_PATH VERSION_STRING)
  #message(FATAL_ERROR "ConfigureMacOSXBundlePlist for ${PROJECT_NAME} ")
  if(CMAKE_BUILD_TYPE MATCHES "Release")
    SET(DBG_EXTENSION "")
  else()
    set(DBG_EXTENSION ${DEBUG_EXTENSION})
  endif()
  get_filename_component(ICON_FILE_NAME "${ICON_FILE_PATH}" NAME)

  set_target_properties(${TARGET_NAME} PROPERTIES
    MACOSX_BUNDLE_INFO_STRING "${TARGET_NAME}${DBG_EXTENSION} Version ${VERSION_STRING}, Copyright 2019 Carnegie Mellon University"
    MACOSX_BUNDLE_ICON_FILE ${ICON_FILE_NAME}
    MACOSX_BUNDLE_GUI_IDENTIFIER "${TARGET_NAME}"
    MACOSX_BUNDLE_LONG_VERSION_STRING "${TARGET_NAME}${DBG_EXTENSION} Version ${VERSION_STRING}"
    MACOSX_BUNDLE_BUNDLE_NAME ${TARGET_NAME}${DBG_EXTENSION}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${VERSION_STRING}
    MACOSX_BUNDLE_BUNDLE_VERSION ${VERSION_STRING}
    MACOSX_BUNDLE_COPYRIGHT "Copyright 2019 Carnegie Mellon University All Rights Reserved."
    MACOSX_BUNDLE_INFO_PLIST ${CMP_OSX_TOOLS_SOURCE_DIR}/MacOSXBundleInfo.plist.in
  )

  SET(${PROJECT_NAME}_PROJECT_SRCS ${${PROJECT_NAME}_PROJECT_SRCS} ${ICON_FILE_PATH})
  SET_SOURCE_FILES_PROPERTIES(${ICON_FILE_PATH} PROPERTIES
                              MACOSX_PACKAGE_LOCATION Resources)
endmacro()

# --------------------------------------------------------------------
# This function should be able to correctly create an Application bundle
# based on the Qt Frameworks for any platform. There are specific
# sections to ensure that plugins and other libraries and resources are
# correctly copied into the Application Bundle. On other platforms these
# items are copied into the installation directory.
# Arguments:
#  TARGET The name of the Target to use in the add_executable() commnad
#  DEBUG_EXTENSION The file name suffix extension that Debug builds will have
#  ICON_FILE The path to the proper icon file for this platform (icns for OS X, ico for windows)
#  VERSION_MAJOR The Major version
#  VERSION_MINOR The Minor version
#  VERSION_PATCH The Patch version
#  BINARY_DIR    The binary directory where some files are created for this application
#  COMPONENT     The name of the component that is used during the packaging
#  INSTALL_DEST  The destination directory inside of the CMAKE_INSTALL_PREFIX to install everything
#
#  SOURCES   All the source files that are needed to compile the code
#  LINK_LIBRARIES Dependent libraries that are needed to properly link the executable
#  LIB_SEARCH_DIRS  A list of directories where certain dependent libraries or plugins can be found
#  QT_PLUGINS A List of Qt Plugins that this project needs
#  OTHER_PLUGINS A list of other plugins that are needed by this Application. These can be those built
#     by this project or located somewhere else.
function(BuildQtAppBundle)
    set(options )
    set(oneValueArgs TARGET DEBUG_EXTENSION ICON_FILE VERSION_MAJOR VERSION_MINOR VERSION_PATCH
                     BINARY_DIR COMPONENT INSTALL_DEST PLUGIN_LIST_FILE)
    set(multiValueArgs SOURCES LINK_LIBRARIES LIB_SEARCH_DIRS QT5_MODULES QT_PLUGINS OTHER_PLUGINS)
    cmake_parse_arguments(QAB "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    # Default GUI type is blank
    set(GUI_TYPE "")

    #-- Configure the OS X Bundle Plist
    if(APPLE)
        SET(GUI_TYPE MACOSX_BUNDLE)
        #-- Make sure the qt_menu.nib is copied if we are using Qt Cocoa by setting the
        # source files properties of the qt_menu.nib package
        if(QT_MAC_USE_COCOA)
            GET_FILENAME_COMPONENT(qt_menu_nib
              "${QT_QTGUI_LIBRARY_RELEASE}/Resources/qt_menu.nib"
              REALPATH)
            set(qt_menu_nib_sources
              "${qt_menu_nib}/classes.nib"
              "${qt_menu_nib}/info.nib"
              "${qt_menu_nib}/keyedobjects.nib"
              )
            SET_SOURCE_FILES_PROPERTIES(
              ${qt_menu_nib_sources}
              PROPERTIES
              MACOSX_PACKAGE_LOCATION Resources/qt_menu.nib
            )
        ELSE(QT_MAC_USE_COCOA)
            set(qt_menu_nib_sources)
        endif(QT_MAC_USE_COCOA)
        list(APPEND QAB_SOURCES ${qt_menu_nib_sources})
    elseif(WIN32)
        SET(GUI_TYPE WIN32)
        FILE (WRITE "${CMAKE_CURRENT_BINARY_DIR}/Icon.rc"
          "// Icon with lowest ID value placed first to ensure application icon\n"
          "// remains consistent on all systems.\n"
          "IDI_ICON1 ICON \"${QAB_ICON_FILE}\"")
        SET(QAB_ICON_FILE "${CMAKE_CURRENT_BINARY_DIR}/Icon.rc")
        cmp_IDE_GENERATED_PROPERTIES("${TARGET}/Generated/QrcFiles" "${QAB_ICON_FILE}" "")
    endif(APPLE)

#-- Append the Icon file/Image/Resource file to the list of Sources to compile
    list(APPEND QAB_SOURCES ${QAB_ICON_FILE})

    foreach(qt5module ${QAB_QT5_MODULES})
      set(QAB_LINK_LIBRARIES ${QAB_LINK_LIBRARIES} Qt5::${qt5module})
    endforeach()

#-- Add and Link our executable
    add_executable( ${QAB_TARGET} ${GUI_TYPE} ${QAB_SOURCES} )
    target_link_libraries( ${QAB_TARGET}
                            ${QAB_LINK_LIBRARIES}
    )
    foreach(qt5module ${QAB_QT5_MODULES})
        target_include_directories(${QAB_TARGET} PUBLIC ${Qt5${qt5module}_INCLUDE_DIRS})
    endforeach()
#-- Make sure we have a proper bundle icon. This must occur AFTER the add_executable command
    if(APPLE)
      ConfigureMacOSXBundlePlist( "${QAB_TARGET}" "${QAB_DEBUG_EXTENSION}" "${QAB_ICON_FILE}"
                                     "${QAB_VERSION_MAJOR}.${QAB_VERSION_MINOR}.${QAB_VERSION_PATCH}" )
    endif(APPLE)

#-- Set the Debug Suffix for the application
    set_target_properties( ${QAB_TARGET}
                PROPERTIES
                DEBUG_OUTPUT_NAME ${QAB_TARGET}${QAB_DEBUG_EXTENSION}
                RELEASE_OUTPUT_NAME ${QAB_TARGET}
    )
    # enable per object parallel compilation in this large library
    if(MSVC)
        target_compile_options(${QAB_TARGET} PRIVATE "/MP")
    endif()
    
    if(CMAKE_SYSTEM_NAME MATCHES "Linux")
        set_target_properties( ${QAB_TARGET}
                PROPERTIES
                INSTALL_RPATH \$ORIGIN/../lib
    )
    endif()
#-- Create install rules for any Qt Plugins that are needed
    set(pi_dest ${QAB_INSTALL_DEST}/Plugins)
    # if we are on OS X then we set the plugin installation location to inside the App bundle
    if(APPLE)
        set(pi_dest ${QAB_TARGET}.app/Contents/Plugins)
        set(osx_app_name ${QAB_TARGET})
        if(CMAKE_BUILD_TYPE MATCHES "Debug")
            set(pi_dest ${QAB_TARGET}${QAB_DEBUG_EXTENSION}.app/Contents/Plugins)
            set(osx_app_name ${QAB_TARGET}${QAB_DEBUG_EXTENSION})
        endif()
    endif()


    set(app_plugin_list "")
    set(lib_search_dirs "")
    set(write_qt_plugins_in_qtconf "")
#-- It is important as you build up the list to modify the path to the Qt Plugin
#-- to point to the plugin that will appear in the Application bundle and NOT
#-- the path to your Qt installation. If you do NOT do this step properly AND you
#-- have write privs on your Qt Installation CMake will most likely "fixup" your
#-- Qt installation files which really isn't good at all. Also when generating the
#-- list it is important to have Absolute Paths to these plugins otherwise
#-- fixup_bundle() can not find the libraries.
    foreach(pi ${QAB_QT_PLUGINS})
        set(write_qt_plugins_in_qtconf "1")
        get_filename_component(qt_plugin_name "${pi}" NAME)
        get_filename_component(qt_plugin_type_path "${pi}" PATH)
        get_filename_component(qt_plugin_type "${qt_plugin_type_path}" NAME)
        #install(PROGRAMS ${pi}
        #        DESTINATION "${pi_dest}/${qt_plugin_type}"
        #        COMPONENT ${QAB_COMPONENT}
        #)
        list(APPEND app_plugin_list "\${CMAKE_INSTALL_PREFIX}/${pi_dest}/${qt_plugin_type}/${qt_plugin_name}")
    endforeach()
    list(REMOVE_DUPLICATES lib_search_dirs)

#
#-- Create install rules for our own plugins that are targets in the build system which
#-- is only needed on Apple systems to make sure we get them installed into the bundle.
#-- On other platforms the standard installation rules are used instead.
    if(APPLE)
        foreach(pi ${QAB_OTHER_PLUGINS})
            get_filename_component(plugin_name "${pi}" NAME)
            install(PROGRAMS ${pi}
                    DESTINATION "${pi_dest}"
                    COMPONENT ${QAB_COMPONENT}
            )
            list(APPEND app_plugin_list "\${CMAKE_INSTALL_PREFIX}/${pi_dest}/${plugin_name}")
        endforeach()
    endif(APPLE)

    #-- Create an Install Rule for the main app bundle target
    install(TARGETS ${QAB_TARGET}
        COMPONENT ${QAB_COMPONENT}
        RUNTIME DESTINATION ${QAB_INSTALL_DEST}
        LIBRARY DESTINATION ${QAB_INSTALL_DEST}
        ARCHIVE DESTINATION ${QAB_INSTALL_DEST}
        BUNDLE DESTINATION ${QAB_INSTALL_DEST}
    )

#-- Create last install rule that will run fixup_bundle() on OS X Machines. Other platforms we
#-- are going to create the install rules elsewhere
    if(APPLE)
        list(APPEND lib_search_dirs "${QAB_LIB_SEARCH_DIRS}")

        set(OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT
                    "${QAB_BINARY_DIR}/OSX_Scripts/${QAB_TARGET}_CompleteBundle.cmake")

        get_property(SIMPLibSearchDirs GLOBAL PROPERTY SIMPLibSearchDirs)


        configure_file("${CMP_OSX_TOOLS_SOURCE_DIR}/CompleteBundle.cmake.in"
                "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" @ONLY IMMEDIATE)

        install(SCRIPT "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" COMPONENT ${QAB_COMPONENT})
    endif(APPLE)

#-- This should be called when we are on Linux
    if(CMAKE_SYSTEM_NAME MATCHES "Linux")
      set(linux_app_name ${QAB_TARGET})
      set(LINUX_MAKE_STANDALONE_LAUNCH_SCRIPT
                  "${QAB_BINARY_DIR}/LINUX_Scripts/${QAB_TARGET}.sh")
      set(lib_suffix "")
      set(build_type "${CMAKE_BUILD_TYPE}")
      if("${build_type}" STREQUAL "Debug")
          set(lib_suffix "_debug")
      endif()

      set(LINUX_INSTALL_LIBS_CMAKE_SCRIPT
              "${QAB_BINARY_DIR}/LINUX_Scripts/${QAB_TARGET}_CompleteBundle.cmake")
      set(OPTIMIZE_BUNDLE_SHELL_SCRIPT
              "${QAB_BINARY_DIR}/LINUX_Scripts/${QAB_TARGET}_InstallLibraries.sh")

      configure_file("${CMP_LINUX_TOOLS_SOURCE_DIR}/CompleteBundle.cmake.in"
                    "${LINUX_INSTALL_LIBS_CMAKE_SCRIPT}" @ONLY IMMEDIATE)
      set(PROJECT_INSTALL_DIR ${linux_app_name})

      configure_file("${CMP_LINUX_TOOLS_SOURCE_DIR}/InstallLibraries.sh.in"
                     "${OPTIMIZE_BUNDLE_SHELL_SCRIPT}" @ONLY IMMEDIATE)

      install(SCRIPT "${LINUX_INSTALL_LIBS_CMAKE_SCRIPT}" COMPONENT ${QAB_COMPONENT})
    endif()


endfunction()

# --------------------------------------------------------------------
# This function should be able to correctly create an Application bundle
# based on the Qt Frameworks for any platform. There are specific
# sections to ensure that plugins and other libraries and resources are
# correctly copied into the Application Bundle. On other platforms these
# items are copied into the installation directory.
# Arguments:
#  TARGET The name of the Target to use in the add_executable() commnad
#  DEBUG_EXTENSION The file name suffix extension that Debug builds will have
#  VERSION_MAJOR The Major version
#  VERSION_MINOR The Minor version
#  VERSION_PATCH The Patch version
#  BINARY_DIR    The binary directory where some files are created for this application
#  COMPONENT     The name of the component that is used during the packaging
#  INSTALL_DEST  The destination directory inside of the CMAKE_INSTALL_PREFIX to install everything
#  SOURCES   All the source files that are needed to compile the code
#  LINK_LIBRARIES Dependent libraries that are needed to properly link the executable
#  LIB_SEARCH_DIRS  A list of directories where certain dependent libraries or plugins can be found
#
# Notes: If we were to base a tool off of Qt and NOT just system/3rd party libraries
#  then we would probably have to get some of the features of the "BuildQtAppBunlde"
#  back in this function in order to copy in the Qt frameworks, plugins and other
#  stuff like that. For now none of our 'tools' require Qt.
function(BuildToolBundle)
    set(options )
    set(oneValueArgs TARGET DEBUG_EXTENSION VERSION_MAJOR VERSION_MINOR VERSION_PATCH
                     BINARY_DIR COMPONENT INSTALL_DEST SOLUTION_FOLDER)
    set(multiValueArgs SOURCES LINK_LIBRARIES LIB_SEARCH_DIRS)
    cmake_parse_arguments(QAB "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    # Default GUI type is blank
    set(GUI_TYPE "")

    if(APPLE)
        set(osx_app_name ${QAB_TARGET})
        if(CMAKE_BUILD_TYPE MATCHES "Debug")
            set(osx_app_name ${QAB_TARGET}${QAB_DEBUG_EXTENSION})
        endif()
    endif()

#-- Add and Link our executable
    add_executable( ${QAB_TARGET} ${GUI_TYPE} ${QAB_SOURCES} )
    target_link_libraries( ${QAB_TARGET}
                            ${QAB_LINK_LIBRARIES} )

#-- Set the Debug Suffix for the application
    set_target_properties( ${QAB_TARGET}
                PROPERTIES
                DEBUG_OUTPUT_NAME ${QAB_TARGET}${QAB_DEBUG_EXTENSION}
                RELEASE_OUTPUT_NAME ${QAB_TARGET}
    )
    if(CMAKE_SYSTEM_NAME MATCHES "Linux")
      set_target_properties( ${QAB_TARGET}
            PROPERTIES
            INSTALL_RPATH \$ORIGIN/../lib )
    endif()
    if(NOT "${QAB_SOLUTION_FOLDER}" STREQUAL "")
      set_target_properties(${QAB_TARGET}
                          PROPERTIES FOLDER ${QAB_SOLUTION_FOLDER})
    endif()

  if( NOT ${QAB_INSTALL_DEST} STREQUAL "")
  #-- Create an Install Rule for the main app bundle target
    install(TARGETS ${QAB_TARGET}
        COMPONENT ${QAB_COMPONENT}
        RUNTIME DESTINATION ${QAB_INSTALL_DEST}
        LIBRARY DESTINATION ${QAB_INSTALL_DEST}
        ARCHIVE DESTINATION ${QAB_INSTALL_DEST}
        BUNDLE DESTINATION ${QAB_INSTALL_DEST}
    )


#-- Create last install rule that will run fixup_bundle() on OS X Machines. Other platforms we
#-- are going to create the install rules elsewhere
    if(0)
        list(APPEND lib_search_dirs "${QAB_LIB_SEARCH_DIRS}")

        set(OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT
            "${QAB_BINARY_DIR}/OSX_Scripts/${QAB_TARGET}_CompleteTool.cmake")
        set(OPTIMIZE_BUNDLE_SHELL_SCRIPT
            "${QAB_BINARY_DIR}/OSX_Scripts/${QAB_TARGET}_OptimizeTool.sh")

        set(PROJECT_INSTALL_DIR "bin")
        configure_file("${CMP_OSX_TOOLS_SOURCE_DIR}/CompleteTool.cmake.in"
                "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" @ONLY IMMEDIATE)

        configure_file("${CMP_OSX_TOOLS_SOURCE_DIR}/CompleteTool.sh.in"
                "${OPTIMIZE_BUNDLE_SHELL_SCRIPT}" @ONLY IMMEDIATE)

        install(SCRIPT "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" COMPONENT ${QAB_COMPONENT})
    endif()
  endif()
endfunction()

# --------------------------------------------------------------------
#
# --------------------------------------------------------------------
macro(cmp_ToolInstallationSupport_old EXE_NAME EXE_DEBUG_EXTENSION EXE_BINARY_DIR installFiles
                                  comp dest lib_search_dirs)
if(0)
    message(STATUS "EXE_NAME: ${EXE_NAME}")
    message(STATUS "EXE_DEBUG_EXTENSION: ${EXE_DEBUG_EXTENSION}")
    message(STATUS "EXE_BINARY_DIR: ${EXE_BINARY_DIR}")
    message(STATUS "appNeedsPlugins: ${appNeedsPlugins}")
    message(STATUS "installFiles: ${installFiles}")
    message(STATUS "comp: ${comp}")
    message(STATUS "dest: ${dest}")
    message(STATUS "lib_search_dirs: ${lib_search_dirs}")
endif()

    set_target_properties( ${EXE_NAME}
        PROPERTIES
        DEBUG_OUTPUT_NAME ${EXE_NAME}${EXE_DEBUG_EXTENSION}
        RELEASE_OUTPUT_NAME ${EXE_NAME}
    )
    if(${installFiles} EQUAL 1)
        install(TARGETS ${EXE_NAME}
            COMPONENT ${comp}
            RUNTIME DESTINATION ${dest}
            LIBRARY DESTINATION ${dest}
            ARCHIVE DESTINATION ${dest}
            BUNDLE DESTINATION  ${dest}
        )

        #   message(STATUS "Creating Install CMake file for tool application ${EXE_NAME}")
        if(APPLE)
            if(CMAKE_BUILD_TYPE MATCHES "Debug")
                MakeOSXTool( "${EXE_NAME}${EXE_DEBUG_EXTENSION}"
                            ${EXE_BINARY_DIR}
                            ${CMP_OSX_TOOLS_SOURCE_DIR}
                            "${dest}"
                            "${lib_search_dirs}")
            else(CMAKE_BUILD_TYPE MATCHES "Debug")
                MakeOSXTool(${EXE_NAME}
                             ${EXE_BINARY_DIR}
                             ${CMP_OSX_TOOLS_SOURCE_DIR}
                             "${dest}"
                             "${lib_search_dirs}")
            endif()
        endif(APPLE)
    endif()
endmacro()

# --------------------------------------------------------------------

macro(LibraryProperties targetName DEBUG_EXTENSION)
    if( NOT BUILD_SHARED_LIBS AND MSVC)
      set_target_properties( ${targetName}
        PROPERTIES
        DEBUG_OUTPUT_NAME lib${targetName}
        RELEASE_OUTPUT_NAME lib${targetName}  )
    endif()

    set_target_properties( ${targetName} PROPERTIES FOLDER ${targetName}Proj)

    #-- Set the Debug and Release names for the libraries
    set_target_properties( ${targetName}
        PROPERTIES
        DEBUG_POSTFIX ${DEBUG_EXTENSION}
    )

    # enable per object parallel compilation in this large library
    if(MSVC)
        target_compile_options(${targetName} PRIVATE "/MP")
    endif()

    if(BUILD_SHARED_LIBS)
      if(APPLE)
        # use, i.e. don't skip the full RPATH for the build tree
        SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

        # when building, don't use the install RPATH already
        # (but later on when installing)
        SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

        SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

        # add the automatically determined parts of the RPATH
        # which point to directories outside the build tree to the install RPATH
        SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

      endif(APPLE)

      if(CMAKE_SYSTEM_NAME MATCHES "Linux")
        set(CMAKE_INSTALL_RPATH "\$ORIGIN/../lib")
        set_target_properties( ${targetName}
                    PROPERTIES
                    INSTALL_RPATH \$ORIGIN/../lib)
      endif()

   endif( BUILD_SHARED_LIBS)

endmacro(LibraryProperties DEBUG_EXTENSION)

# --------------------------------------------------------------------
macro(StaticLibraryProperties targetName )
    if(WIN32 AND NOT MINGW)
        set(DEBUG_EXTENSION "_d")
    else()
        set(DEBUG_EXTENSION "_debug")
    endif()


    if(WIN32 AND NOT MINGW)
        SET(LIBRARY_RELEASE_NAME "lib${targetName}" CACHE INTERNAL "" FORCE)
        SET(LIBRARY_DEBUG_NAME "lib${targetName}${DEBUG_EXTENSION}" CACHE INTERNAL "" FORCE)
    else(WIN32 AND NOT MINGW)
        SET(LIBRARY_RELEASE_NAME "${targetName}" CACHE INTERNAL "" FORCE)
        SET(LIBRARY_DEBUG_NAME "${targetName}${DEBUG_EXTENSION}" CACHE INTERNAL "" FORCE)
    endif(WIN32 AND NOT MINGW)


    #-- Set the Debug and Release names for the libraries
    set_target_properties( ${targetName}
        PROPERTIES
        DEBUG_OUTPUT_NAME ${LIBRARY_DEBUG_NAME}
        RELEASE_OUTPUT_NAME ${LIBRARY_RELEASE_NAME}
    )

endmacro(StaticLibraryProperties)

#-------------------------------------------------------------------------------
# This is used if you are creating a plugin that needs to be installed
#-------------------------------------------------------------------------------
function(PluginProperties)
    set(options )
    set(oneValueArgs TARGET_NAME DEBUG_EXTENSION VERSION LIB_SUFFIX FOLDER OUTPUT_NAME BINARY_DIR PLUGIN_FILE INSTALL_DEST)
    set(multiValueArgs )
    cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
    
    string(REPLACE "."
       "_" ARCHIVE_SUFFIX
       "${Z_LIB_SUFFIX}")

    #-- Set the Debug and Release names for the libraries
    set_target_properties( ${Z_TARGET_NAME}
        PROPERTIES
        DEBUG_POSTFIX ${Z_DEBUG_EXTENSION}
        SUFFIX ${Z_LIB_SUFFIX}
        FOLDER ${Z_FOLDER}Plugin
        OUTPUT_NAME ${Z_OUTPUT_NAME}
        ARCHIVE_OUTPUT_NAME ${Z_OUTPUT_NAME}${ARCHIVE_SUFFIX}
    )

    if( NOT BUILD_SHARED_LIBS AND MSVC)
      set_target_properties( ${Z_TARGET_NAME}
        PROPERTIES
        DEBUG_OUTPUT_NAME lib${Z_OUTPUT_NAME}
        RELEASE_OUTPUT_NAME lib${Z_OUTPUT_NAME} 
        )
    endif()

    if(NOT MSVC)
        set_target_properties (${Z_TARGET_NAME} PROPERTIES
            LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/Plugins    
        )
    endif()

    # Add the plugin to our list of plugins that will need to be installed
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(Z_BUILD_TYPE ${Z_DEBUG_EXTENSION})
    endif()
    if(NOT MSVC)
        file(APPEND ${Z_PLUGIN_FILE} "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/Plugins/lib${Z_OUTPUT_NAME}${Z_BUILD_TYPE}${Z_LIB_SUFFIX};")
    else()
        file(APPEND ${Z_PLUGIN_FILE} "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/lib${Z_OUTPUT_NAME}${Z_LIB_SUFFIX};")
    endif()

    if(NOT APPLE)
        set(BUILD_TYPES "Debug;Release")
        foreach(btype ${BUILD_TYPES})
            install(TARGETS ${Z_TARGET_NAME}
                    DESTINATION ${Z_INSTALL_DEST}
                    CONFIGURATIONS ${btype}
                    COMPONENT Applications
                    ARCHIVE DESTINATION lib
                    )
        endforeach()
    endif()

    # --------------------------------------------------------------------
    # Add in some compiler definitions
    # --------------------------------------------------------------------
    if( CMAKE_BUILD_TYPE MATCHES Debug )
        target_compile_definitions(${Z_TARGET_NAME} PRIVATE -DDEBUG)
    endif( CMAKE_BUILD_TYPE MATCHES Debug )

    # On linux we need to set this because some of the libraries are Static
    # and some are shared.
    if( CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64" AND NOT MSVC )
        target_compile_options(${Z_TARGET_NAME} PRIVATE -fPIC)
    endif()

    # --------------------------------------------------------------------
    # If we are using GCC, make the compiler messages on a single line
    if(CMAKE_COMPILER_IS_GNUCC)
        target_compile_options(${Z_TARGET_NAME} PRIVATE -fmessage-length=0)
    endif(CMAKE_COMPILER_IS_GNUCC)
    if(CMAKE_COMPILER_IS_GNUCXX)
        target_compile_options(${Z_TARGET_NAME} PRIVATE -fmessage-length=0)
    endif(CMAKE_COMPILER_IS_GNUCXX)

    if(MSVC AND SIMPL_DISABLE_MSVC_WARNINGS)
        target_compile_definitions(${Z_TARGET_NAME} PRIVATE -D_CRT_SECURE_NO_WARNINGS)
        target_compile_definitions(${Z_TARGET_NAME} PRIVATE -D_SCL_SECURE_NO_WARNINGS)
    endif()

    # enable per object parallel compilation in this large library
    if(MSVC)
        target_compile_options(${Z_TARGET_NAME} PRIVATE "/MP")
    endif()

endfunction()

# --------------------------------------------------------------------
#-- Copy all the dependent DLLs into the current build directory so that the test
#-- can run.
macro(CMP_COPY_DEPENDENT_LIBRARIES _libraryList)
#  message(STATUS "#--------------------------------------------")
#  message(STATUS "CMP_COPY_DEPENDENT_LIBRARIES: ${_libraryList}")
  set(SUPPORT_LIB_OPTION 1)
  if(MSVC_IDE)
    set(SUPPORT_LIB_OPTION 0)
  elseif(APPLE) # Apple systems do NOT need this so just skip this entirely
    set(SUPPORT_LIB_OPTION 2)
  elseif(UNIX AND NOT MSVC)
    set(SUPPORT_LIB_OPTION 3)
  endif()

  set(_libraryList ${_libraryList})
  set(TYPES Debug Release)
  if( ${SUPPORT_LIB_OPTION} EQUAL 1)
    set(TYPES ${CMAKE_BUILD_TYPE})
  endif()

  if(SUPPORT_LIB_OPTION EQUAL 0 OR SUPPORT_LIB_OPTION EQUAL 1)
    # Make all the necessary intermediate directories for Visual Studio
    if(MSVC_IDE)
      file(MAKE_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Debug)
      file(MAKE_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Release)
      file(MAKE_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/MinSizeRel)
      file(MAKE_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/RelWithDebInfo)
    endif()
    FOREACH(lib ${_libraryList})

      STRING(TOUPPER ${lib} upperlib)
      # message(STATUS "upperlib: ${upperlib}")
      # message(STATUS "${upperlib}_IS_SHARED: ${${upperlib}_IS_SHARED}")
      if(${upperlib}_IS_SHARED)
        FOREACH(BTYPE ${TYPES})
          # Smessage(STATUS "Looking for ${BTYPE} DLL Version of ${lib}")
          STRING(TOUPPER ${BTYPE} TYPE)
          get_filename_component(lib_path ${${upperlib}_LIBRARY_${TYPE}} PATH)
          get_filename_component(lib_name ${${upperlib}_LIBRARY_${TYPE}} NAME_WE)
          #message(STATUS "lib_path: ${lib_path}")
          #message(STATUS "lib_name: ${lib_name}")
          #message(STATUS "${upperlib}_BIN_DIR: ${${upperlib}_BIN_DIR}")

          find_file(${upperlib}_LIBRARY_DLL_${TYPE}
                        NAMES ${lib_name}.dll
                        PATHS  ${lib_path}/../bin ${lib_path}/.. ${lib_path}/ ${${upperlib}_BIN_DIR}
                        NO_DEFAULT_PATH )
          # message(STATUS "${upperlib}_LIBRARY_DLL_${TYPE}: ${${upperlib}_LIBRARY_DLL_${TYPE}}")
          mark_as_advanced(${upperlib}_LIBRARY_DLL_${TYPE})
          if( ${${upperlib}_LIBRARY_DLL_${TYPE}} STREQUAL  "${upperlib}_LIBRARY_DLL_${TYPE}-NOTFOUND")
            message(FATAL_ERROR "According to how ${upperlib}_LIBRARY_${TYPE} was found the library should"
                                " have been built as a DLL but no .dll file can be found. I looked in the "
                                " following locations:\n  ${lib_path}\n  ${lib_path}/..\n  ${lib_path}/../bin\n  ${${upperlib}_BIN_DIR}")
          endif()

         # SET(${upperlib}_LIBRARY_DLL_${TYPE} "${${upperlib}_LIBRARY_DLL_${TYPE}}/${lib_name}.dll" CACHE FILEPATH "The path to the DLL Portion of the library" FORCE)
         # message(STATUS "${upperlib}_LIBRARY_DLL_${TYPE}: ${${upperlib}_LIBRARY_DLL_${TYPE}}")
          #message(STATUS "Generating Copy Rule for DLL: ${${upperlib}_LIBRARY_DLL_${TYPE}}")
          if(SUPPORT_LIB_OPTION EQUAL 1)
            set(BTYPE ".")
          endif()
          if(NOT TARGET ZZ_${upperlib}_DLL_${TYPE}-Copy)
            ADD_CUSTOM_TARGET(ZZ_${upperlib}_DLL_${TYPE}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${${upperlib}_LIBRARY_DLL_${TYPE}}
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BTYPE}/
                            COMMENT "  Copy: ${${upperlib}_LIBRARY_DLL_${TYPE}}\n    To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BTYPE}/")
            set_target_properties(ZZ_${upperlib}_DLL_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/${upperlib})
            get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
            set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${upperlib}_DLL_${TYPE}-Copy) 
          endif()
        ENDFOREACH(BTYPE ${TYPES})
      endif(${upperlib}_IS_SHARED)
    ENDFOREACH(lib ${_libraryList})
  endif()
endmacro()

# --------------------------------------------------------------------
# This macro generates install rules for Visual Studio builds so that
# dependent DLL libraries (HDF5, Tiff, Expat, DataModel) will be
# properly installed with your project.
# --------------------------------------------------------------------
macro(CMP_LIBRARIES_INSTALL_RULES _libraryList destination)
#  message(STATUS "CMP_LIBRARIES_INSTALL_RULES")
  set(_libraryList ${_libraryList})
  set(TYPES Debug Release)
  if(MSVC)
    FOREACH(lib ${_libraryList})
        STRING(TOUPPER ${lib} upperlib)

        FOREACH(BTYPE ${TYPES} )
          STRING(TOUPPER ${BTYPE} TYPE)
          get_filename_component(lib_path ${${upperlib}_LIBRARY_${TYPE}} PATH)
          get_filename_component(lib_name ${${upperlib}_LIBRARY_${TYPE}} NAME_WE)

          find_file(${upperlib}_LIBRARY_DLL_${TYPE}
                        NAMES ${lib_name}.dll
                        PATHS  ${lib_path}/../bin ${lib_path}/.. ${lib_path}/ ${${upperlib}_BIN_DIR}
                        NO_DEFAULT_PATH )
         # message(STATUS "${upperlib}_LIBRARY_DLL_${TYPE}: ${${upperlib}_LIBRARY_DLL_${TYPE}}")
          mark_as_advanced(${upperlib}_LIBRARY_DLL_${TYPE})
          if( ${${upperlib}_LIBRARY_DLL_${TYPE}} STREQUAL  "${upperlib}_LIBRARY_DLL_${TYPE}-NOTFOUND")
             message(STATUS "A Companion DLL for ${upperlib}_LIBRARY_${TYPE} was NOT found which usually means\n"
                                " that the library was NOT built as a DLL. I looked in the \n"
                                " following locations:\n  ${lib_path}\n  ${lib_path}/..\n  ${lib_path}/../bin\n  ${${upperlib}_BIN_DIR}")
          else()
             # set(${upperlib}_LIBRARY_DLL_${TYPE}  ${${upperlib}_LIBRARY_DLL_${TYPE}}/${lib_name}.dll)
           #   message(STATUS "${upperlib}_LIBRARY_DLL_${TYPE}: ${${upperlib}_LIBRARY_DLL_${TYPE}}")
           #   message(STATUS "Generating Install Rule for DLL Library ${${upperlib}_LIBRARY_DLL_${TYPE}}")
              install(FILES ${${upperlib}_LIBRARY_DLL_${TYPE}}
                DESTINATION ${destination}
                CONFIGURATIONS ${BTYPE}
                COMPONENT Applications)
          endif()

        ENDFOREACH(BTYPE ${TYPES})
    ENDFOREACH(lib ${_libraryList})
  endif(MSVC)

#-- This will create install rules for the dylibs on linux hopefully creating
#-- a stand alone .zip or .tgz file
    if(UNIX AND NOT APPLE)
      FOREACH(lib ${_libraryList})
        STRING(TOUPPER ${lib} upperlib)

        set(BTYPE "RELEASE" )
          STRING(TOUPPER ${BTYPE} TYPE)
	  message(STATUS "${upperlib}_LIBRARY_${TYPE}: ${${upperlib}_LIBRARY_${TYPE}}")
          get_filename_component(lib_path "${${upperlib}_LIBRARY_${TYPE}}" PATH)
          get_filename_component(lib_name "${${upperlib}_LIBRARY_${TYPE}}" NAME_WE)

          find_file(${upperlib}_LIBRARY_SO_${TYPE}
                        NAMES ${lib_name}.so
                        PATHS  ${lib_path}/../bin ${lib_path}/.. ${lib_path}/ ${${upperlib}_BIN_DIR}
                        NO_DEFAULT_PATH )
         # message(STATUS "${upperlib}_LIBRARY_SO_${TYPE}: ${${upperlib}_LIBRARY_SO_${TYPE}}")
          mark_as_advanced(${upperlib}_LIBRARY_SO_${TYPE})
          if( "${${upperlib}_LIBRARY_SO_${TYPE}}" STREQUAL  "${upperlib}_LIBRARY_SO_${TYPE}-NOTFOUND")
             message(STATUS "A shared library for ${upperlib}_LIBRARY_${TYPE} was NOT found which usually means\n"
                                " that the library was NOT built as a .so. I looked in the \n"
                                " following locations:\n  ${lib_path}\n  ${lib_path}/..\n  ${lib_path}/../bin\n  ${${upperlib}_BIN_DIR}")
          else()
           #   message(STATUS "${upperlib}_LIBRARY_SO_${TYPE}: ${${upperlib}_LIBRARY_SO_${TYPE}}")
           #   message(STATUS "Generating Install Rule for .so Library ${${upperlib}_LIBRARY_SO_${TYPE}}")
              install(FILES ${${upperlib}_LIBRARY_SO_${TYPE}}
                DESTINATION "lib"
                CONFIGURATIONS ${BTYPE}
                COMPONENT Applications)
          endif()
      ENDFOREACH(lib ${_libraryList})
    endif()
ENDmacro()

#-------------------------------------------------------------------------------
# This macro will attempt a try_run command in order to compile and then
# generate a version string based on today's date. The output string should be
# of the form YYYY.MM.DD.
#  Required CMake variables to be set are:
#   Project_CMAKE_DIR - The path to the Project CMake directory
#  The following variables are set, all of which should have been already
#  initialized to a default value
#   ${CMP_PROJECT_NAME}_VERSION
#   ${CMP_PROJECT_NAME}_VER_MAJOR
#   ${CMP_PROJECT_NAME}_VER_MINOR
#   ${CMP_PROJECT_NAME}_VER_PATCH
#
#-------------------------------------------------------------------------------

function(cmpGenerateVersionString)
  set(options)
  set(oneValueArgs GENERATED_HEADER_FILE_PATH GENERATED_SOURCE_FILE_PATH
                   NAMESPACE PROJECT_NAME EXPORT_MACRO )
  cmake_parse_arguments(GVS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  if(0)
    message(STATUS "--------------------------------------------")
    message(STATUS "GVS_NAMESPACE: ${GVS_NAMESPACE}")
    message(STATUS "GVS_PROJECT_NAME: ${GVS_PROJECT_NAME}")
    message(STATUS "GVS_GENERATED_HEADER_FILE_PATH: ${GVS_GENERATED_HEADER_FILE_PATH}")
    message(STATUS "GVS_GENERATED_SOURCE_FILE_PATH: ${GVS_GENERATED_SOURCE_FILE_PATH}")
    message(STATUS "GVS_PROJECT_SOURCE_DIR: ${GVS_PROJECT_SOURCE_DIR}")
    message(STATUS "GVS_PROJECT_VERSION_MAJOR: ${GVS_PROJECT_VERSION_MAJOR}")
    message(STATUS "${GVS_PROJECT_NAME}_BUILD_DATE: ${${GVS_PROJECT_NAME}_BUILD_DATE}")
  endif()

  set(PROJECT_PREFIX "${GVS_PROJECT_NAME}")
  set(VERSION_GEN_NAME "${GVS_PROJECT_NAME}")
  set(VERSION_GEN_NAMESPACE "${GVS_NAMESPACE}")
  string(TOLOWER VERSION_GEN_NAMESPACE VERSION_INCLUDE_GUARD)
  set(VERSION_GEN_NAMESPACE_EXPORT "${GVS_EXPORT_MACRO}")
  set(VERSION_GEN_VER_MAJOR  ${${GVS_PROJECT_NAME}_VERSION_MAJOR})
  set(VERSION_GEN_VER_MINOR  ${${GVS_PROJECT_NAME}_VERSION_MINOR})
  set(VERSION_GEN_VER_PATCH "0")
  set(VERSION_GEN_VER_REVISION "0")
  set(VERSION_BUILD_DATE ${${GVS_PROJECT_NAME}_BUILD_DATE})
  set(VERSION_GEN_HEADER_FILE_NAME ${GVS_GENERATED_HEADER_FILE_PATH})

  set(${GVS_PROJECT_NAME}_VERSION_PATCH "${VERSION_GEN_VER_PATCH}" PARENT_SCOPE)
  set(${GVS_PROJECT_NAME}_VERSION_TWEAK "${VERSION_GEN_VER_REVISION}" PARENT_SCOPE)

  cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_HEADER_FILE_PATH}
                                CONFIGURED_TEMPLATE_PATH   ${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersion.h.in )

  cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_SOURCE_FILE_PATH}
                                CONFIGURED_TEMPLATE_PATH   ${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersion.cpp.in )

endfunction()

#-------------------------------------------------------------------------------
# This function generates a file ONLY if the MD5 between the "to be" generated file
# and the current file are different. This will help reduce recompiles based on
# the generation of files that are really the same.
#
function(cmpConfigureFileWithMD5Check)
    set(options)
    set(oneValueArgs CONFIGURED_TEMPLATE_PATH GENERATED_FILE_PATH )
    cmake_parse_arguments(GVS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

 #   message(STATUS "   GVS_CONFIGURED_TEMPLATE_PATH: ${GVS_CONFIGURED_TEMPLATE_PATH}")
 #   message(STATUS "   GVS_GENERATED_FILE_PATH: ${GVS_GENERATED_FILE_PATH}")

    # Only Generate a file if it is different than what is already there.
    if(EXISTS ${GVS_GENERATED_FILE_PATH} )
        file(MD5 ${GVS_GENERATED_FILE_PATH} VERSION_HDR_MD5)
        configure_file(${GVS_CONFIGURED_TEMPLATE_PATH}   ${GVS_GENERATED_FILE_PATH}_tmp  @ONLY)

        file(MD5 ${GVS_GENERATED_FILE_PATH}_tmp VERSION_GEN_HDR_MD5)
        #message(STATUS "  File Exists, doing MD5 Comparison")

        # Compare the MD5 checksums. If they are different then configure the file into the proper location
        if(NOT "${VERSION_HDR_MD5}" STREQUAL "${VERSION_GEN_HDR_MD5}")
            #message(STATUS "   ${VERSION_GEN_HDR_MD5}")
            #message(STATUS "   ${VERSION_HDR_MD5}")
            #message(STATUS "  Files differ: Replacing with newly generated file")
            configure_file(${GVS_CONFIGURED_TEMPLATE_PATH}  ${GVS_GENERATED_FILE_PATH} @ONLY)
        else()
            #message(STATUS "  NO Difference in Files")
        endif()
        file(REMOVE ${GVS_GENERATED_FILE_PATH}_tmp)
    else()
      # message(STATUS "  File does NOT Exist, Generating one...")
      configure_file(${GVS_CONFIGURED_TEMPLATE_PATH} ${GVS_GENERATED_FILE_PATH} @ONLY)
    endif()

endfunction()

#-------------------------------------------------------------------------------
# This function generates a file ONLY if the MD5 between the "to be" generated file
# and the current file are different. This will help reduce recompiles based on
# the generation of files that are really the same.
#
function(cmpReplaceFileIfDifferent)
    set(options)
    set(oneValueArgs NEW_FILE_PATH OLD_FILE_PATH )
    cmake_parse_arguments(GVS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

   # message(STATUS "GVS_NEW_FILE_PATH: ${GVS_NEW_FILE_PATH}")
   # message(STATUS "GVS_OLD_FILE_PATH: ${GVS_OLD_FILE_PATH}")

    # Only Generate a file if it is different than what is already there.

    if(EXISTS ${GVS_OLD_FILE_PATH} )
      #  message(STATUS "  File Exists, doing MD5 Comparison")
        file(MD5 ${GVS_OLD_FILE_PATH} VERSION_HDR_MD5)
        file(MD5 ${GVS_NEW_FILE_PATH} VERSION_GEN_HDR_MD5)

        # Compare the MD5 checksums. If they are different then configure the file into the proper location
        if(NOT "${VERSION_HDR_MD5}" STREQUAL "${VERSION_GEN_HDR_MD5}")
        #    message(STATUS "  Files differ: Replacing with newly generated file")
            file(REMOVE ${GVS_OLD_FILE_PATH})
            file(RENAME ${GVS_NEW_FILE_PATH} ${GVS_OLD_FILE_PATH})

        else()
         #   message(STATUS "  NO Difference in Files")
            file(REMOVE ${GVS_NEW_FILE_PATH})
        endif()

    else()
       # message(STATUS "  File does NOT Exist, Generating one...")
        file(RENAME ${GVS_NEW_FILE_PATH} ${GVS_OLD_FILE_PATH})
    endif()

endfunction()


#-------------------------------------------------------------------------------
# This function will attempt to generate a build date/time string.
#
#-------------------------------------------------------------------------------
function(cmpGenerateBuildDate)
  set(oneValueArgs PROJECT_NAME )
  cmake_parse_arguments(GVS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  IF (WIN32)
      EXECUTE_PROCESS(COMMAND "cmd" " /C date /T" OUTPUT_VARIABLE RESULT)
      string(REPLACE  " " ";" RESULT ${RESULT})
      list(GET RESULT 1 RESULT)
      string(REPLACE "/" ";" RESULT ${RESULT})
      list(LENGTH RESULT LIST_LENGTH)
      if(LIST_LENGTH GREATER 2)
        list(GET RESULT 2 YEAR)
        list(GET RESULT 0 MONTH)
        list(GET RESULT 1 DAY)
        set(${GVS_PROJECT_NAME}_BUILD_DATE "${YEAR}/${MONTH}/${DAY}" PARENT_SCOPE)
      else()
        set(${GVS_PROJECT_NAME}_BUILD_DATE "0000/00/00" PARENT_SCOPE)
      endif()
      #message(STATUS "${GVS_PROJECT_NAME}_BUILD_DATE: ${${GVS_PROJECT_NAME}_BUILD_DATE}")
  ELSEIF(UNIX)
      EXECUTE_PROCESS(COMMAND "date" "+%Y/%m/%d/" OUTPUT_VARIABLE RESULT)
      string(REPLACE "/" ";" RESULT ${RESULT})
      list(LENGTH RESULT LIST_LENGTH)
      if(LIST_LENGTH GREATER 2)
        list(GET RESULT 0 YEAR)
        list(GET RESULT 1 MONTH)
        list(GET RESULT 2 DAY)
        set(${GVS_PROJECT_NAME}_BUILD_DATE "${YEAR}/${MONTH}/${DAY}" PARENT_SCOPE)
      else()
          set(${GVS_PROJECT_NAME}_BUILD_DATE "0000/00/00" PARENT_SCOPE)
      endif()
      #message(STATUS "${GVS_PROJECT_NAME}_BUILD_DATE: ${${GVS_PROJECT_NAME}_BUILD_DATE}")
  ELSE (WIN32)
      MESSAGE(SEND_ERROR "date for this operating system not implemented")
      set(${GVS_PROJECT_NAME}_BUILD_DATE "0000/00/00" PARENT_SCOPE)
  ENDIF (WIN32)

endfunction()



#-------------------------------------------------------------------------------
# We are going to use Git functionality to create a version number for our package
# The specific functionality we are going to use is the 'git describe' function
# which should return the latest tag, the number commits since that tag and the
# SHA1 of that commit. If we fail to find git then we fall back to a manually
# entered version number.
function(cmpGitRevisionString)
  set(options)
  set(oneValueArgs GENERATED_HEADER_FILE_PATH GENERATED_SOURCE_FILE_PATH
                   NAMESPACE PROJECT_NAME EXPORT_MACRO VERSION_MACRO_PATH)
  cmake_parse_arguments(GVS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  if(0)
    message(STATUS "--------------------------------------------")
    message(STATUS "GVS_NAMESPACE: ${GVS_NAMESPACE}")
    message(STATUS "GVS_PROJECT_NAME: ${GVS_PROJECT_NAME}")
    message(STATUS "GVS_GENERATED_HEADER_FILE_PATH: ${GVS_GENERATED_HEADER_FILE_PATH}")
    message(STATUS "GVS_GENERATED_SOURCE_FILE_PATH: ${GVS_GENERATED_SOURCE_FILE_PATH}")
    message(STATUS "GVS_PROJECT_SOURCE_DIR: ${GVS_PROJECT_SOURCE_DIR}")
    message(STATUS "GVS_PROJECT_VERSION_MAJOR: ${GVS_PROJECT_VERSION_MAJOR}")
    message(STATUS "GVS_EXPORT_MACRO: ${GVS_EXPORT_MACRO}")
    message(STATUS "${GVS_PROJECT_NAME}_BUILD_DATE: ${${GVS_PROJECT_NAME}_BUILD_DATE}")
    message(STATUS "${GVS_PROJECT_NAME}_SOURCE_DIR: ${${GVS_PROJECT_NAME}_SOURCE_DIR}")
    message(STATUS "--------------------------------------------")
  endif()


  # Run 'git describe' to get our tag offset
  execute_process(COMMAND ${GIT_EXECUTABLE} describe --long
                  OUTPUT_VARIABLE DVERS
                  RESULT_VARIABLE did_run
                  ERROR_VARIABLE git_error
                  WORKING_DIRECTORY ${${GVS_PROJECT_NAME}_SOURCE_DIR} )

  #message(STATUS "DVERS: ${DVERS}")
  set(PROJECT_PREFIX "${GVS_PROJECT_NAME}")
  set(VERSION_GEN_NAME "${GVS_PROJECT_NAME}")
  set(VERSION_GEN_NAMESPACE "${GVS_NAMESPACE}")
  string(TOLOWER "${VERSION_GEN_NAMESPACE}" VERSION_INCLUDE_GUARD)
  set(VERSION_GEN_NAMESPACE_EXPORT "${GVS_EXPORT_MACRO}")
  set(VERSION_GEN_VER_MAJOR  ${${GVS_PROJECT_NAME}_VERSION_MAJOR})
  set(VERSION_GEN_VER_MINOR  ${${GVS_PROJECT_NAME}_VERSION_MINOR})
  set(VERSION_GEN_VER_PATCH "0")
  set(VERSION_GEN_VER_REVISION "0")
  set(VERSION_BUILD_DATE ${${GVS_PROJECT_NAME}_BUILD_DATE})
  set(VERSION_GEN_HEADER_FILE_NAME ${GVS_GENERATED_HEADER_FILE_PATH})

  #-- Make sure that actually worked and if not just generate some dummy values
  if(DVERS STREQUAL "")
    message(STATUS "[${GVS_PROJECT_NAME}] DVERS was Empty. Generating Default version strings")
  else()
    string(STRIP ${DVERS} DVERS)
    string(REPLACE  "-" ";" VERSION_LIST ${DVERS})
    list(LENGTH VERSION_LIST VERSION_LIST_LENGTH)

    set(VERSION_GEN_VER_PATCH "0")
    set(VERSION_GEN_VER_REVISION "0")

    list(LENGTH VERSION_LIST LIST_LENGTH)
    if(LIST_LENGTH GREATER 1)
      list(GET VERSION_LIST 1 VERSION_GEN_VER_PATCH)
      list(GET VERSION_LIST 2 VERSION_GEN_VER_REVISION)
    endif()

    string(SUBSTRING ${VERSION_GEN_VER_REVISION} 1 -1 VERSION_GEN_VER_REVISION)

  endif()

  set(${GVS_PROJECT_NAME}_VERSION_PATCH "${VERSION_GEN_VER_PATCH}" PARENT_SCOPE)
  set(${GVS_PROJECT_NAME}_VERSION_TWEAK "${VERSION_GEN_VER_REVISION}" PARENT_SCOPE)
  set(CMP_TOP_HEADER_INCLUDE_STATMENT "")
  if(NOT "${CMP_TOP_HEADER_FILE}" STREQUAL "")
    set(CMP_TOP_HEADER_INCLUDE_STATMENT "#include \"${CMP_TOP_HEADER_FILE}\"")
  endif()
  if(NOT "${GVS_GENERATED_HEADER_FILE_PATH}" STREQUAL "")
    #message(STATUS "Generating: ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_HEADER_FILE_PATH}")
    cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_HEADER_FILE_PATH}
                                CONFIGURED_TEMPLATE_PATH   ${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersion.h.in )
  endif()
  
  if(NOT "${GVS_GENERATED_SOURCE_FILE_PATH}" STREQUAL "")
    #message(STATUS "Generating: ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_SOURCE_FILE_PATH}")
    cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_SOURCE_FILE_PATH}
                                CONFIGURED_TEMPLATE_PATH   ${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersion.cpp.in )
  endif()
  
  if(NOT "${GVS_VERSION_MACRO_PATH}" STREQUAL "")
    #message(STATUS "Generating: ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_VERSION_MACRO_PATH}")
    cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_VERSION_MACRO_PATH}
                                CONFIGURED_TEMPLATE_PATH   ${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersionMacro.h.in )
  endif()
  
endfunction()

#-------------------------------------------------------------------------------
# We are going to use Git functionality to create a version number for our package
# The specific functionality we are going to use is the 'git describe' function
# which should return the latest tag, the number commits since that tag and the
# SHA1 of that commit. If we fail to find git then we fall back to a manually
# entered version number.
function(cmpManualRevisionString)
  set(options)
  set(oneValueArgs GENERATED_HEADER_FILE_PATH GENERATED_SOURCE_FILE_PATH
                   NAMESPACE PROJECT_NAME EXPORT_MACRO VERSION_MACRO_PATH)
  cmake_parse_arguments(GVS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  if(0)
    message(STATUS "--------------------------------------------")
    message(STATUS "GVS_NAMESPACE: ${GVS_NAMESPACE}")
    message(STATUS "GVS_PROJECT_NAME: ${GVS_PROJECT_NAME}")
    message(STATUS "GVS_GENERATED_HEADER_FILE_PATH: ${GVS_GENERATED_HEADER_FILE_PATH}")
    message(STATUS "GVS_GENERATED_SOURCE_FILE_PATH: ${GVS_GENERATED_SOURCE_FILE_PATH}")
    message(STATUS "GVS_PROJECT_SOURCE_DIR: ${GVS_PROJECT_SOURCE_DIR}")
    message(STATUS "GVS_PROJECT_VERSION_MAJOR: ${GVS_PROJECT_VERSION_MAJOR}")
    message(STATUS "GVS_EXPORT_MACRO: ${GVS_EXPORT_MACRO}")
    message(STATUS "${GVS_PROJECT_NAME}_BUILD_DATE: ${${GVS_PROJECT_NAME}_BUILD_DATE}")
    message(STATUS "${GVS_PROJECT_NAME}_SOURCE_DIR: ${${GVS_PROJECT_NAME}_SOURCE_DIR}")
    message(STATUS "--------------------------------------------")
  endif()


  #message(STATUS "DVERS: ${DVERS}")
  set(PROJECT_PREFIX "${GVS_PROJECT_NAME}")
  set(VERSION_GEN_NAME "${GVS_PROJECT_NAME}")
  set(VERSION_GEN_NAMESPACE "${GVS_NAMESPACE}")
  string(TOLOWER "${VERSION_GEN_NAMESPACE}" VERSION_INCLUDE_GUARD)
  set(VERSION_GEN_NAMESPACE_EXPORT "${GVS_EXPORT_MACRO}")
  set(VERSION_GEN_VER_MAJOR  ${${GVS_PROJECT_NAME}_VERSION_MAJOR})
  set(VERSION_GEN_VER_MINOR  ${${GVS_PROJECT_NAME}_VERSION_MINOR})
  set(VERSION_GEN_VER_PATCH ${${GVS_PROJECT_NAME}_VERSION_PATCH})
  set(VERSION_GEN_VER_REVISION ${${GVS_PROJECT_NAME}_VERSION_TWEAK})
  set(VERSION_BUILD_DATE ${${GVS_PROJECT_NAME}_BUILD_DATE})
  set(VERSION_GEN_HEADER_FILE_NAME ${GVS_GENERATED_HEADER_FILE_PATH})

  set(${GVS_PROJECT_NAME}_VERSION_PATCH "${VERSION_GEN_VER_PATCH}" PARENT_SCOPE)
  set(${GVS_PROJECT_NAME}_VERSION_TWEAK "${VERSION_GEN_VER_REVISION}" PARENT_SCOPE)
  set(CMP_TOP_HEADER_INCLUDE_STATMENT "")
  if(NOT "${CMP_TOP_HEADER_FILE}" STREQUAL "")
    set(CMP_TOP_HEADER_INCLUDE_STATMENT "#include \"${CMP_TOP_HEADER_FILE}\"")
  endif()
  if(NOT "${GVS_GENERATED_HEADER_FILE_PATH}" STREQUAL "")
    #message(STATUS "Generating: ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_HEADER_FILE_PATH}")
    cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_HEADER_FILE_PATH}
                                CONFIGURED_TEMPLATE_PATH   ${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersion.h.in )
  endif()
  
  if(NOT "${GVS_GENERATED_SOURCE_FILE_PATH}" STREQUAL "")
    #message(STATUS "Generating: ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_SOURCE_FILE_PATH}")
    cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_GENERATED_SOURCE_FILE_PATH}
                                CONFIGURED_TEMPLATE_PATH   ${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersion.cpp.in )
  endif()
  
  if(NOT "${GVS_VERSION_MACRO_PATH}" STREQUAL "")
    #message(STATUS "Generating: ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_VERSION_MACRO_PATH}")
    cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${${GVS_PROJECT_NAME}_BINARY_DIR}/${GVS_VERSION_MACRO_PATH}
                                CONFIGURED_TEMPLATE_PATH   ${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersionMacro.h.in )
  endif()
  
endfunction()


#-------------------------------------------------------------------------------
#
#
#
function(cmpRevisionString)
  set(options) 
  set(oneValueArgs GENERATED_HEADER_FILE_PATH GENERATED_SOURCE_FILE_PATH GENERATED_MACRO_HEADER_PATH
                   NAMESPACE PROJECT_NAME EXPORT_MACRO )
  cmake_parse_arguments(GVS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  # Generate our Build date in the Form of YYYY/MM/DD
  cmpGenerateBuildDate(PROJECT_NAME ${GVS_PROJECT_NAME})

  cmpManualRevisionString( GENERATED_HEADER_FILE_PATH "${GVS_GENERATED_HEADER_FILE_PATH}"
                          GENERATED_SOURCE_FILE_PATH "${GVS_GENERATED_SOURCE_FILE_PATH}"
                          VERSION_MACRO_PATH "${GVS_GENERATED_MACRO_HEADER_PATH}"
                          NAMESPACE "${GVS_NAMESPACE}"
                          PROJECT_NAME "${GVS_PROJECT_NAME}"
                          EXPORT_MACRO "${GVS_EXPORT_MACRO}")


  # We have to "set" our variable into the parent scope. What a pain. Pass by reference would be really nice about now
  set(${GVS_PROJECT_NAME}_VERSION_PATCH "${${GVS_PROJECT_NAME}_VERSION_PATCH}" PARENT_SCOPE)
  set(${GVS_PROJECT_NAME}_VERSION_TWEAK "${${GVS_PROJECT_NAME}_VERSION_TWEAK}" PARENT_SCOPE)
  set(${GVS_PROJECT_NAME}_BUILD_DATE "${${GVS_PROJECT_NAME}_BUILD_DATE}" PARENT_SCOPE)

  if(0)
    message(STATUS "${GVS_PROJECT_NAME}_VERSION_MAJOR: ${${GVS_PROJECT_NAME}_VERSION_MAJOR}")
    message(STATUS "${GVS_PROJECT_NAME}_VERSION_MINOR: ${${GVS_PROJECT_NAME}_VERSION_MINOR}")
    message(STATUS "${GVS_PROJECT_NAME}_VERSION_PATCH: ${${GVS_PROJECT_NAME}_VERSION_PATCH}")
    message(STATUS "${GVS_PROJECT_NAME}_VERSION_TWEAK: ${${GVS_PROJECT_NAME}_VERSION_TWEAK}")
    message(STATUS "${GVS_PROJECT_NAME}_BUILD_DATE: ${${GVS_PROJECT_NAME}_BUILD_DATE}")
  endif()
endfunction()

#-------------------------------------------------------------------------------
# Function COMPILE_TOOL to help alleviate lots of extra code below for adding
# simple command line tools that just need one or two source files
function(COMPILE_TOOL)
    set(options)
    set(oneValueArgs TARGET DEBUG_EXTENSION BINARY_DIR COMPONENT INSTALL_DEST DEFINITION)
    set(multiValueArgs SOURCES LINK_LIBRARIES)
    cmake_parse_arguments(D3DTOOL "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    if( ${D3DTOOL_DEFINITION} )
    
    endif()

    BuildToolBundle(
        TARGET ${D3DTOOL_TARGET}
        SOURCES ${D3DTOOL_SOURCES}
        DEBUG_EXTENSION ${D3DTOOL_DEBUG_EXTENSION}
        VERSION_MAJOR ${D3DTOOL_DREAM3D_VER_MAJOR}
        VERSION_MINOR ${D3DTOOL_DREAM3D_VER_MINOR}
        VERSION_PATCH ${D3DTOOL_DREAM3D_VER_PATCH}
        BINARY_DIR    ${D3DTOOL_BINARY_DIR}
        LINK_LIBRARIES ${D3DTOOL_LINK_LIBRARIES}
        LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
        COMPONENT     Applications
        INSTALL_DEST  "${D3DTOOL_INSTALL_DEST}"
    )
    target_compile_definitions(${D3DTOOL_TARGET} -D${DEFINITION})

endfunction()

# --------------------------------------------------------------------------
# Converts file paths to use the '/' character so they are compatible with
# C/C++ language. The use of the "\" character would make the compiler think
# the following character would be escaped.
#-- Convert all '\' to '\\' so that they are properly escaped in the header file
macro(ConvertPathToHeaderCompatible INPUT)
    if(WIN32)
      STRING(REPLACE "\\" "\\\\" ${INPUT} ${${INPUT}} )
      STRING(REPLACE "/" "\\\\" ${INPUT} ${${INPUT}}  )
    endif()
endmacro()

#-------------------------------------------------------------------------------
# 
function(CMP_MODULE_INCLUDE_DIRS)
  set(options)
  set(oneValueArgs TARGET LIBVARS)
  set(multiValueArgs )
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  foreach(LIB ${Z_LIBVARS})
    target_include_directories(${Z_TARGET} PUBLIC ${${LIB}_INCLUDE_DIRS})
    target_include_directories(${Z_TARGET} PUBLIC ${${LIB}_INCLUDE_DIR})
  endforeach()


endfunction()

#-------------------------------------------------------------------------------
#
function(CMP_ADD_Qt5_INCLUDE_DIR)
  set(options)
  set(oneValueArgs TARGET)
  set(multiValueArgs COMPONENTS)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  #-- Make sure we include the proper Qt5 include directories
  foreach(qtlib ${Z_COMPONENTS})
    target_include_directories(${Z_TARGET} PUBLIC ${Qt5${qtlib}_INCLUDE_DIRS})
  endforeach()

endfunction()


#-------------------------------------------------------------------------------
#
function(CMP_AddDefinitions)
  set(options)
  set(oneValueArgs TARGET)
  set(multiValueArgs)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  # --------------------------------------------------------------------
  # Add in some compiler definitions
  # --------------------------------------------------------------------
  if( CMAKE_BUILD_TYPE MATCHES Debug )
    target_compile_definitions(${Z_TARGET} PRIVATE -DDEBUG)
  endif( CMAKE_BUILD_TYPE MATCHES Debug )

  # On linux we need to set this because some of the libraries are Static
  # and some are shared.
  if( CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64" AND NOT MSVC )
    target_compile_options(${Z_TARGET} PRIVATE -fPIC)
  endif()

  # --------------------------------------------------------------------
  # If was are using GCC, make the compiler messages on a single line
  if(CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(${Z_TARGET} PRIVATE -fmessage-length=0)
  endif(CMAKE_COMPILER_IS_GNUCC)
  if(CMAKE_COMPILER_IS_GNUCXX)
    target_compile_options(${Z_TARGET} PRIVATE -fmessage-length=0)
  endif(CMAKE_COMPILER_IS_GNUCXX)

  if(MSVC AND SIMPL_DISABLE_MSVC_WARNINGS)
    target_compile_definitions(${Z_TARGET} PRIVATE -D_CRT_SECURE_NO_WARNINGS)
    target_compile_definitions(${Z_TARGET} PRIVATE -D_SCL_SECURE_NO_WARNINGS)
  endif()

endfunction()


# --------------------------------------------------------------------------
# Adds a Unit Test 
function(AddSIMPLUnitTest)
    set(options)
    set(oneValueArgs TESTNAME FOLDER)
    set(multiValueArgs SOURCES LINK_LIBRARIES INCLUDE_DIRS)
    cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    add_executable( ${Z_TESTNAME} ${Z_SOURCES})
    if("${Z_FOLDER}" STREQUAL "")
        set(Z_FOLDER "Test")
    endif()
    set_target_properties( ${Z_TESTNAME} PROPERTIES FOLDER ${Z_FOLDER})
    CMP_AddDefinitions(TARGET ${Z_TESTNAME} )
    cmp_IDE_SOURCE_PROPERTIES( "" "" "${Z_SOURCES}" "0")
    target_include_directories(${Z_TESTNAME} PUBLIC ${Z_INCLUDE_DIRS})
    target_link_libraries( ${Z_TESTNAME} ${Z_LINK_LIBRARIES})
    add_test(${Z_TESTNAME} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${Z_TESTNAME})

endfunction()


