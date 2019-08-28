# --------------------------------------------------------------------
#-- Copy all the Qt5 dependent DLLs into the current build directory so that
#-- one can debug an application or library that depends on Qt5 libraries.
#-- This macro is really intended for Windows Builds because windows libraries
#-- do not have any type of rpath or install_name encoded in the libraries so
#-- the least intrusive way to deal with the PATH issues is to just copy all
#-- the dependend DLL libraries into the build directory. Note that this is
#-- NOT needed for static libraries.
function(CopyQt5RunTimeLibraries)

  set(options)
  set(oneValueArgs PREFIX)
  set(multiValueArgs LIBRARIES)

  cmake_parse_arguments(P "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  # message(STATUS "Copying Qt5 Runtime Libraries: ${P_LIBRARIES}")
  set(SUPPORT_LIB_OPTION 1)
  if(MSVC_IDE)
    set(SUPPORT_LIB_OPTION 0)
  elseif(APPLE) # Apple systems do NOT need this so just skip this entirely
    set(SUPPORT_LIB_OPTION 2)
  elseif(UNIX AND NOT MSVC)
    set(SUPPORT_LIB_OPTION 3)
  endif()

  GET_FILENAME_COMPONENT(QT_DLL_PATH_tmp ${QtQMake_location} PATH)

  if(SUPPORT_LIB_OPTION EQUAL 0)
    # message(STATUS "SUPPORT_LIB_OPTION = 0")

    foreach(qtlib ${P_LIBRARIES})
      set(TYPE "d")
      # message(STATUS "Copy Rule for Qt library ${P_PREFIX}${qtlib}${TYPE}.dll")
      # We need to copy both the Debug and Release versions of the libraries into their respective
      # subfolders for Visual Studio builds
      if(NOT TARGET ZZ_${P_PREFIX}${qtlib}${TYPE}-Debug-Copy)
        add_custom_target(ZZ_${P_PREFIX}${qtlib}${TYPE}-Debug-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${QT_DLL_PATH_tmp}/${P_PREFIX}${qtlib}${TYPE}.dll
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Debug/
                            # COMMENT "Copying ${P_PREFIX}${qtlib}${TYPE}.dll to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Debug/"
                            )
        set_target_properties(ZZ_${P_PREFIX}${qtlib}${TYPE}-Debug-Copy PROPERTIES FOLDER ZZ_COPY_FILES/Debug/Qt5)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${P_PREFIX}${qtlib}${TYPE}-Debug-Copy)
      endif()

    #   message(STATUS "Generating Copy Rule for Qt Release DLL Library ${QT_DLL_PATH_tmp}/${qtlib}d.dll")
      set(TYPE "")
      if(NOT TARGET ZZ_${P_PREFIX}${qtlib}${TYPE}-Release-Copy)
        add_custom_target(ZZ_${P_PREFIX}${qtlib}${TYPE}-Release-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${QT_DLL_PATH_tmp}/${P_PREFIX}${qtlib}.dll
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Release/
                            # COMMENT "Copying ${P_PREFIX}${qtlib}.dll to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Release/"
                            )
        set_target_properties(ZZ_${P_PREFIX}${qtlib}${TYPE}-Release-Copy PROPERTIES FOLDER ZZ_COPY_FILES/Release/Qt5)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${P_PREFIX}${qtlib}${TYPE}-Release-Copy)
      endif()
    endforeach(qtlib)
  elseif(SUPPORT_LIB_OPTION EQUAL 1)
    # This will get hit if Ninja, MinGW, MSYS, Cygwin is used for the build system
    # message(STATUS "SUPPORT_LIB_OPTION = 1")

    foreach(qtlib ${P_LIBRARIES})
      set(TYPE "")
      if( "${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
          set(TYPE "d")
      else()

      endif()

      # message(STATUS "Copy Rule for Qt library ${P_PREFIX}${qtlib}${TYPE}.dll")
      # We need to copy the library into the "Bin" Folder
      if(NOT TARGET ZZ_${qtlib}-Copy)
        add_custom_target(ZZ_${qtlib}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${QT_DLL_PATH_tmp}/${P_PREFIX}${qtlib}${TYPE}.dll
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
                            COMMENT "Copying ${P_PREFIX}${qtlib}${TYPE}.dll to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/")
        set_target_properties(ZZ_${qtlib}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/Qt5)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${qtlib}-Copy)
      endif()
    endforeach(qtlib)
  endif()
endfunction()


# -------------------------------------------------------------
# This function adds the necessary cmake code to find the HDF5
# shared libraries and setup custom copy commands and/or install
# rules for Linux and Windows to use
function(AddQt5SupportLibraryCopyInstallRules)
  set(options )
  set(oneValueArgs PREFIX DEBUG_SUFFIX)
  set(multiValueArgs LIBRARIES)
  cmake_parse_arguments(P "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  set(SUPPORT_LIB_OPTION 1)
  if(MSVC_IDE)
    set(SUPPORT_LIB_OPTION 0)
  elseif(APPLE) # Apple systems do NOT need this so just skip this entirely
    set(SUPPORT_LIB_OPTION 2)
  elseif(UNIX AND NOT MSVC)
    set(SUPPORT_LIB_OPTION 3)
  endif()

  GET_FILENAME_COMPONENT(QT_DLL_PATH ${QtQMake_location} PATH)

  if(WIN32)
    set(destination "bin")
  else()
    set(destination "lib")
  endif()


  if(SUPPORT_LIB_OPTION EQUAL 0)
    # We need to copy both the Debug and Release versions of the libraries into their respective
    # subfolders for Visual Studio builds
    foreach(qtlib ${P_LIBRARIES})

      set(INT_DIR "Debug")
      # message(STATUS "Copy Rule for Qt Support library ${qtlib}${SUFFIX}.dll")
      if(NOT TARGET ZZ_${qtlib}-${INT_DIR}-Copy)
        set(SUFFIX ${P_DEBUG_SUFFIX})
        add_custom_target(ZZ_${qtlib}-${INT_DIR}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${QT_DLL_PATH}/${P_PREIX}${qtlib}${SUFFIX}.dll
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/
                            COMMENT "Copying ${P_PREIX}${qtlib}${SUFFIX}.dll to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/")
        set_target_properties(ZZ_${qtlib}-${INT_DIR}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${INT_DIR}/Qt5)
        install(FILES ${QT_DLL_PATH}/${P_PREIX}${qtlib}${SUFFIX}.dll  DESTINATION "${destination}" CONFIGURATIONS ${INT_DIR} COMPONENT Applications)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${qtlib}-${INT_DIR}-Copy)
      endif()

      set(INT_DIR "Release")
      if(NOT TARGET ZZ_${qtlib}-${INT_DIR}-Copy)
        set(SUFFIX "")
        add_custom_target(ZZ_${qtlib}-${INT_DIR}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${QT_DLL_PATH}/${P_PREIX}${qtlib}${SUFFIX}.dll
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/
                            COMMENT "Copying ${P_PREIX}${qtlib}${SUFFIX}.dll to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/")
        set_target_properties(ZZ_${qtlib}-${INT_DIR}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${INT_DIR}/Qt5)
        install(FILES ${QT_DLL_PATH}/${P_PREIX}${qtlib}${SUFFIX}.dll  DESTINATION "${destination}" CONFIGURATIONS ${INT_DIR} COMPONENT Applications)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${qtlib}-${INT_DIR}-Copy)      
      endif()
    endforeach(qtlib)

  elseif(SUPPORT_LIB_OPTION EQUAL 1)
  # This should be the code path for Ninja/NMake/Makefiles all on NON-OS X systems
    set(SUFFIX "")
    if( "${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
        set(SUFFIX ${P_DEBUG_SUFFIX})
    endif()
    set(INT_DIR "")
    foreach(qtlib ${P_LIBRARIES})
      # message(STATUS "Copy Rule for Qt Support library ${P_PREIX}${qtlib}${SUFFIX}.dll")
      # We need to copy the library into the "Bin" folder
      if(NOT TARGET ZZ_${qtlib}-Copy)
        add_custom_target(ZZ_${qtlib}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${QT_DLL_PATH}/${P_PREIX}${qtlib}${SUFFIX}.dll
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
                            COMMENT "Copying ${P_PREIX}${qtlib}${SUFFIX}.dll to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
        set_target_properties(ZZ_${qtlib}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${INT_DIR}/Qt5)
        install(FILES ${QT_DLL_PATH}/${P_PREIX}${qtlib}${SUFFIX}.dll  DESTINATION "${destination}" CONFIGURATIONS ${CMAKE_BUILD_TYPE} COMPONENT Applications)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${qtlib}-Copy)        
      endif()
    endforeach(qtlib)

  endif()

endfunction()

# --------------------------------------------------------------------
#
#
# --------------------------------------------------------------------
function(AddQt5LibraryInstallRule)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs LIBRARIES)

  cmake_parse_arguments(P "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  # message(STATUS "Install Rules Qt5 Libraries: ${P_LIBRARIES}")

  set(build_types "Debug;Release")
  if(WIN32)
    set(qt_plugin_prefix "")
    set(qt_plugin_DEBUG_suffix "d")
    set(qt_plugin_RELEASE_suffix "")
    set(destination "bin")
  else()
    set(qt_plugin_prefix "lib")
    set(qt_plugin_DEBUG_suffix "_debug")
    set(qt_plugin_RELEASE_suffix "")
    set(destination "lib")
  endif()

  foreach(qtlib ${P_LIBRARIES})
    foreach(build_type ${build_types})
      get_target_property(${build_type}_loc Qt5::${qtlib} LOCATION_${build_type})
      if(NOT APPLE)
        install(FILES ${${build_type}_loc}
                DESTINATION "${destination}"
                CONFIGURATIONS ${build_type}
                COMPONENT Applications)
      endif()
    endforeach()
  endforeach()

endfunction()



#-------------------------------------------------------------------------------
# Finds plugins from the Qt installation. The pluginlist argument should be
# something like "QDDS QGif QICNS QICO QJp2 QJpeg QMng QTga QTiff QWbmp QWebp"
#-------------------------------------------------------------------------------
function(AddQt5Plugins)
  set(options)
  set(oneValueArgs PLUGIN_FILE PLUGIN_FILE_TEMPLATE PLUGIN_TYPE PLUGIN_SUFFIX)
  set(multiValueArgs PLUGIN_NAMES)
  if("${CMAKE_BUILD_TYPE}" STREQUAL "")
    set(CMAKE_BUILD_TYPE "Release")
  endif()
  cmake_parse_arguments(P "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(build_types "Debug;Release")
  if(WIN32)
    set(qt_plugin_prefix "")
    set(qt_plugin_DEBUG_suffix "d")
    set(qt_plugin_RELEASE_suffix "")
  else()
    set(qt_plugin_prefix "lib")
    set(qt_plugin_DEBUG_suffix "_debug")
    set(qt_plugin_RELEASE_suffix "")
  endif()

  # message(STATUS "Qt5 Plugins: ${P_PLUGIN_NAMES}")

  foreach(build_type ${build_types})
    # message(STATUS "build_type: ${build_type}")
    foreach(plugin ${P_PLUGIN_NAMES})
      if(TARGET Qt5::${plugin}${P_PLUGIN_SUFFIX})
        get_target_property(${build_type}_loc Qt5::${plugin}${P_PLUGIN_SUFFIX} LOCATION_${build_type})
        # We only use install rules for Linux/Windows.
        # OS X will get its own installation script that moves the Plugins correctly into the bundle
        if(NOT APPLE)
          install(FILES ${${build_type}_loc}
              DESTINATION ./Plugins/${P_PLUGIN_TYPE}
              CONFIGURATIONS ${build_type}
              COMPONENT Applications)
        endif()
      endif()
    endforeach()
  endforeach()

  # Assign either the debug or release plugin list to the QTPLUGINS variable on NON msvc platforms.
  if(NOT MSVC_IDE)
    if( ${CMAKE_BUILD_TYPE} STREQUAL "Debug")
        set(QTPLUGINS ${QTPLUGINS_DEBUG})
    else()
        set(QTPLUGINS ${QTPLUGINS_RELEASE})
    endif()
  endif()

  if(NOT APPLE)
      set(QTCONF_DIR "bin")
      set(QTPLUGINS_DIR "../")
      if(WIN32)
        set(QTCONF_DIR "bin")
        set(QTPLUGINS_DIR "../")
      endif()

      # Create the qt.conf file so that the image plugins will be loaded correctly
      FILE(WRITE ${PROJECT_BINARY_DIR}/qt.conf "[Paths]\nPlugins = ${QTPLUGINS_DIR}Plugins\n")
      FILE(APPEND ${PROJECT_BINARY_DIR}/qt.conf "Prefix = .\n")
      FILE(APPEND ${PROJECT_BINARY_DIR}/qt.conf "LibraryExecutables = .\n")
      FILE(APPEND ${PROJECT_BINARY_DIR}/qt.conf "Data = .\n")

      install(FILES ${PROJECT_BINARY_DIR}/qt.conf
              DESTINATION ${QTCONF_DIR}
              COMPONENT Applications)
  endif()


  #file(APPEND ${P_LIBRARY_SEARCH_FILE} "${QT_PLUGINS_DIR}/${plugintype};")
endfunction()

# ------------------------------------------------------------------------------
#  Function AddQWebEngineSupportFiles
# ------------------------------------------------------------------------------
function(AddQWebEngineSupportFiles)
  set(options)
  set(oneValueArgs QT_INSTALL_PREFIX QT_VERSION)
  set(multiValueArgs )
  if("${CMAKE_BUILD_TYPE}" STREQUAL "")
    set(CMAKE_BUILD_TYPE "Release")
  endif()
  cmake_parse_arguments(QM "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(build_types "Debug;Release")

  set(QTCONF_DIR "bin")
  if(WIN32)
    set(QTCONF_DIR ".")
  endif()

  if (QM_QT_VERSION VERSION_GREATER 5.8.0 OR QM_QT_VERSION VERSION_EQUAL 5.8.0 )
    message(FATAL_ERROR "Qt 5.8.x is not supported for development.")
  elseif (QM_QT_VERSION VERSION_GREATER 5.7.0 OR QM_QT_VERSION VERSION_EQUAL 5.7.0 )
    message(FATAL_ERROR "Qt 5.7 is not supported for development.")
  elseif (QM_QT_VERSION VERSION_GREATER 5.6.0 OR QM_QT_VERSION VERSION_EQUAL 5.6.0)
    message(FATAL_ERROR "Qt 5.6.x is not supported for development.")
  elseif (QM_QT_VERSION VERSION_GREATER 5.5.0 OR QM_QT_VERSION VERSION_EQUAL 5.5.0)
    message(FATAL_ERROR "Qt 5.5.x is not supported for development.")
  elseif (QM_QT_VERSION VERSION_GREATER 5.4.0 OR QM_QT_VERSION VERSION_EQUAL 5.4.0)
    message(FATAL_ERROR "Qt 5.4.x is not supported for development.")
  endif()
  
  if(APPLE)

  endif()

endfunction()

# ------------------------------------------------------------------------------
# Macro CMP_AddQt5Support
# Qt 5 Section: This section is the base cmake code that will find Qt5 on the computer and
# setup all the necessary Qt5 modules that need to be used.
#  @param Qt5Components These are the Qt Components that the project needs. The
#    possible values are: Core Widgets Network Gui Concurrent Script Svg Xml OpenGL PrintSupport
#    Note that one OR more components can be selected.
#  @param NeedQtWebEngine Does the project need QWebEngine. Possible values are TRUE or FALSE
#  @param ProjectBinaryDir The Directory where to write any output files
# ------------------------------------------------------------------------------
macro(CMP_AddQt5Support Qt5Components NeedQtWebEngine ProjectBinaryDir VarPrefix)

  # ------------------------------------------------------------------------------
  # Find includes in corresponding build directories
  set(CMAKE_INCLUDE_CURRENT_DIR ON)

  # Find the QtWidgets library
  set(Qt5_COMPONENTS "${Qt5Components}")
  set(Qt5_WebEngine_Components "")
  if("${NeedQtWebEngine}" STREQUAL "ON" OR "${NeedQtWebEngine}" STREQUAL "TRUE")
    set(Qt5_WebEngine_Components
      Positioning
      WebEngine
      WebEngineWidgets
      WebEngineCore
      WebChannel
      Quick
      QuickWidgets
      Qml
    )
    set(Qt5_COMPONENTS ${Qt5_COMPONENTS} ${Qt5_WebEngine_Components})
  endif()

  # This line gets the list of Qt5 components into the calling context.
  set(${VarPrefix}_Qt5_Components ${Qt5_COMPONENTS} CACHE STRING "" FORCE)

  # On Linux we need the DBus library
  if(CMAKE_SYSTEM_NAME MATCHES "Linux")
    set(Qt5_COMPONENTS ${Qt5_COMPONENTS} DBus)
  endif()

  find_package(Qt5 COMPONENTS ${Qt5_COMPONENTS})
  if(NOT Qt5_FOUND)
    message(FATAL_ERROR "Qt5 is Required for ${PROJECT_NAME} to build. Please install it.")
  endif()

  # We need the location of QMake for later on in order to find the plugins directory
  get_target_property(QtQMake_location Qt5::qmake LOCATION)
  get_property(Qt5_STATUS_PRINTED GLOBAL PROPERTY Qt5_STATUS_PRINTED)
  execute_process(COMMAND "${QtQMake_location}" -query QT_INSTALL_PREFIX OUTPUT_VARIABLE QM_QT_INSTALL_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND "${QtQMake_location}" -query QT_VERSION OUTPUT_VARIABLE QM_QT_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE)

  if(NOT Qt5_STATUS_PRINTED)
    message(STATUS "Qt5 Location: ${QM_QT_INSTALL_PREFIX}")
    message(STATUS "Qt5 Version: ${QM_QT_VERSION} ")
    set_property(GLOBAL PROPERTY Qt5_STATUS_PRINTED TRUE)
  endif()

  # This is really just needed for Windows
  CopyQt5RunTimeLibraries(LIBRARIES ${Qt5_COMPONENTS} PREFIX Qt5)

  # This is pretty much needed on all the platforms.
  AddQt5LibraryInstallRule(LIBRARIES ${Qt5_COMPONENTS})

  #----------------------------------------------------------------------------
  # This is a wierd way to bracket the Qt releases but there has NEVER been 
  # an official version of Qt with a 5.x.99 version so we are going to use that
  # range to bracket each release of Qt that we Support.
  # QT 5.6.x
  if (QM_QT_VERSION VERSION_GREATER 5.5.99 AND QM_QT_VERSION VERSION_LESS 5.6.99)
    set(Qt5_ICU_COMPONENTS icudt54 icuin54 icuuc54)
  endif()

  # QT 5.7.x
  if (QM_QT_VERSION VERSION_GREATER 5.6.99 AND QM_QT_VERSION VERSION_LESS 5.7.99
      AND NOT (CMAKE_SYSTEM_NAME MATCHES "Linux"))
    set(Qt5_ICU_COMPONENTS icudt54 icuin54 icuuc54)
    message(WARNING "Qt 5.7 has never been tested. It may not package correctly")
  endif()

  # QT 5.8.x
  if (QM_QT_VERSION VERSION_GREATER 5.7.99 AND QM_QT_VERSION VERSION_LESS 5.8.99)
    set(Qt5_ICU_COMPONENTS "")
  endif()

  # QT 5.9.x
  if (QM_QT_VERSION VERSION_GREATER 5.8.99 AND QM_QT_VERSION VERSION_LESS 5.9.99)
    set(Qt5_ICU_COMPONENTS "")
  endif()

  # QT 5.10.x
  if (QM_QT_VERSION VERSION_GREATER 5.9.99 AND QM_QT_VERSION VERSION_LESS 5.10.99)
    set(Qt5_ICU_COMPONENTS "")
  endif()

  if(CMAKE_SYSTEM_NAME MATCHES "Linux")
    set(Qt5_ICU_COMPONENTS icui18n icuuc icudata)
  endif()
  # Each Platform has a set of support libraries that need to be copied
  AddQt5SupportLibraryCopyInstallRules( LIBRARIES ${Qt5_ICU_COMPONENTS} PREFIX "" DEBUG_SUFFIX "")

  #----------------------------------------------------------------------------
  # Windows needs OpenGL component libraries copied into the build directory
  if(WIN32)
    set(Qt5_OGL_COMPONENTS libEGL libGLESv2 opengl32sw)
    AddQt5SupportLibraryCopyInstallRules( LIBRARIES ${Qt5_OGL_COMPONENTS} PREFIX "" DEBUG_SUFFIX "")
  endif()
  #-- Let CMake determine which files need to have 'moc' run on them
  set(CMAKE_AUTOMOC TRUE)

  set_property(GLOBAL PROPERTY AUTOGEN_TARGETS_FOLDER "Qt5AutoMocTargets")

  get_property(QT_PLUGINS_FILE GLOBAL PROPERTY QtPluginsTxtFile)
  if("${QT_PLUGINS_FILE}" STREQUAL "")
#    message(STATUS "Setting GLOBAL PROPERTY QtPluginsTxtFile")
    set_property(GLOBAL PROPERTY QtPluginsTxtFile "${ProjectBinaryDir}/Qt_Plugins.txt")
    get_property(QT_PLUGINS_FILE GLOBAL PROPERTY QtPluginsTxtFile)
  endif()

  #set(QT_PLUGINS_FILE_TEMPLATE "${ProjectBinaryDir}/Qt_Plugins.cmake.in")
  get_property(QT_PLUGINS_FILE_TEMPLATE GLOBAL PROPERTY QtPluginsCMakeFile)
  if("${QT_PLUGINS_FILE_TEMPLATE}" STREQUAL "")
#    message(STATUS "Setting GLOBAL PROPERTY QtPluginsCMakeFile")
    set_property(GLOBAL PROPERTY QtPluginsCMakeFile "${ProjectBinaryDir}/Qt_Plugins.cmake.in")
    get_property(QT_PLUGINS_FILE_TEMPLATE GLOBAL PROPERTY QtPluginsCMakeFile)
  endif()



  file(WRITE ${QT_PLUGINS_FILE_TEMPLATE} "")
  file(WRITE ${QT_PLUGINS_FILE} "")


  list(FIND Qt5_COMPONENTS "Gui" NeedsGui)
  if(NeedsGui GREATER -1)
    AddQt5Plugins(PLUGIN_NAMES QDDS QGif QICNS QICO QJp2 QJpeg QMng QTga QTiff QWbmp QWebp
                PLUGIN_FILE "${QT_PLUGINS_FILE}"
                PLUGIN_FILE_TEMPLATE "${QT_PLUGINS_FILE_TEMPLATE}"
                PLUGIN_SUFFIX Plugin
                PLUGIN_TYPE imageformats)
    if(WIN32)
      AddQt5Plugins(PLUGIN_NAMES QWindowsIntegration
                  PLUGIN_FILE "${QT_PLUGINS_FILE}"
                  PLUGIN_FILE_TEMPLATE "${QT_PLUGINS_FILE_TEMPLATE}"
                  PLUGIN_SUFFIX Plugin
                  PLUGIN_TYPE platforms)
      AddQt5Plugins(PLUGIN_NAMES QWindowsDirect2DIntegration
                  PLUGIN_FILE "${QT_PLUGINS_FILE}"
                  PLUGIN_FILE_TEMPLATE "${QT_PLUGINS_FILE_TEMPLATE}"
                  PLUGIN_SUFFIX Plugin
                  PLUGIN_TYPE platforms)
      AddQt5Plugins(PLUGIN_NAMES QMinimalIntegration
                  PLUGIN_FILE "${QT_PLUGINS_FILE}"
                  PLUGIN_FILE_TEMPLATE "${QT_PLUGINS_FILE_TEMPLATE}"
                  PLUGIN_SUFFIX Plugin
                  PLUGIN_TYPE platforms)
       AddQt5Plugins(PLUGIN_NAMES QOffscreenIntegration
                  PLUGIN_FILE "${QT_PLUGINS_FILE}"
                  PLUGIN_FILE_TEMPLATE "${QT_PLUGINS_FILE_TEMPLATE}"
                  PLUGIN_SUFFIX Plugin
                  PLUGIN_TYPE platforms)

      AddQt5Plugins(PLUGIN_NAMES QWindowsVistaStyle
                  PLUGIN_FILE "${QT_PLUGINS_FILE}"
                  PLUGIN_FILE_TEMPLATE "${QT_PLUGINS_FILE_TEMPLATE}"
                  PLUGIN_SUFFIX Plugin
                  PLUGIN_TYPE styles)
    endif()

    if(CMAKE_SYSTEM_NAME MATCHES "Linux")
      AddQt5Plugins(PLUGIN_NAMES QXcbIntegration
                  PLUGIN_FILE "${QT_PLUGINS_FILE}"
                  PLUGIN_FILE_TEMPLATE "${QT_PLUGINS_FILE_TEMPLATE}"
                  PLUGIN_SUFFIX Plugin
                  PLUGIN_TYPE platforms)
    endif()
  endif()

  #-----------------------------------------------------------------------------------
  # Copy over the proper QWebEngine Components
  if("${NeedQtWebEngine}" STREQUAL "ON" OR "${NeedQtWebEngine}" STREQUAL "TRUE")
    AddQWebEngineSupportFiles(QT_INSTALL_PREFIX ${QM_QT_INSTALL_PREFIX} QT_VERSION ${QM_QT_VERSION})
  endif()

  if(0)
    AddQt5Plugins(PLUGIN_NAMES AccessibleFactory
                PLUGIN_FILE "${QT_PLUGINS_FILE}"
                PLUGIN_FILE_TEMPLATE "${QT_PLUGINS_FILE_TEMPLATE}"
                PLUGIN_TYPE accessible)
  endif()

  # Append the locations of the Qt libraries to our Library Search Paths
  list(APPEND CMP_LIB_SEARCH_DIRS ${QT_BINARY_DIR} ${QT_LIBRARY_DIR} )

endmacro()


