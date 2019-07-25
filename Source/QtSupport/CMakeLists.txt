# set project's name
PROJECT( QtSupport VERSION ${EMsoft_VERSION_MAJOR}.${EMsoft_VERSION_MINOR})

set(PROJECT_CODE_DIR ${PROJECT_SOURCE_DIR}/Source)
if(NOT PROJECT_RESOURCES_DIR)
  set(PROJECT_RESOURCES_DIR ${QtSupport_SOURCE_DIR}/Resources)
endif()
if(NOT DEFINED CMP_SOURCE_DIR)
  set(CMP_SOURCE_DIR ${QtSupport_SOURCE_DIR}/Support/cmp)
endif()

set(PROJECT_PREFIX "QtSupport" CACHE STRING "The Prefix to be used for Preprocessor definitions")
set(QtSupport_INSTALL_FILES 0)
# -----------------------------------------------------------------------
# The project generally does NOT want to install headers. See if another
# project has over ridden that property
get_property(CMP_PROJECT_INSTALL_HEADERS GLOBAL PROPERTY CMP_PROJECT_INSTALL_HEADERS)
if("${CMP_PROJECT_INSTALL_HEADERS}" STREQUAL "")
  set_property(GLOBAL PROPERTY CMP_PROJECT_INSTALL_HEADERS "0")
  get_property(CMP_PROJECT_INSTALL_HEADERS GLOBAL PROPERTY CMP_PROJECT_INSTALL_HEADERS)
endif()
set(QtSupport_INSTALL_FILES "${CMP_PROJECT_INSTALL_HEADERS}")

set(QtSupport_BUILT_AS_DYNAMIC_LIB)
if(BUILD_SHARED_LIBS)
    set(QtSupport_BUILT_AS_DYNAMIC_LIB 1)
endif(BUILD_SHARED_LIBS)

#-------------------------------------------------------------------------------
# Include the cmp project to perform all the basic configuration tests for the
# project.
#-------------------------------------------------------------------------------
set(CMP_HEADER_DIR ${${PROJECT_NAME}_BINARY_DIR})
set(CMP_CONFIGURATION_FILE_NAME "${PROJECT_NAME}Configuration.h")
set(CMP_EXPORT_FILE_NAME "${PROJECT_NAME}DLLExport.h")
set(CMP_TYPES_FILE_NAME "${PROJECT_NAME}Types.h")
set(CMP_VERSION_HEADER_FILE_NAME "${PROJECT_NAME}Version.h")
set(CMP_VERSION_SOURCE_FILE_NAME "${PROJECT_NAME}Version.cpp")
set(CMP_TOP_HEADER_FILE "${PROJECT_NAME}.h")
set(CMP_PROJECT_NAMESPACE "${PROJECT_NAME}")
set(CMP_PROJECT_NAME "${PROJECT_NAME}")
set(CMP_ENABLE_PLUGINS "0")
set(CMP_LIB_SEARCH_DIRS "")
set(CMP_GENERATE_VERSION_STRING 1)
set(PROJECT_INSTALL_HEADERS "${CMP_PROJECT_INSTALL_HEADERS}")
set(EXE_DEBUG_EXTENSION "_debug")

INCLUDE (${CMP_SOURCE_DIR}/cmpProject.cmake)

#-------------------------------------------------------------------------------
# Set our H5Lite codes to also compile the Qt API version of H5Lite
#-------------------------------------------------------------------------------
set(QtSupport_USE_QT 0)
option(QtSupport_INCLUDE_QT_API "Include support for using Qt classes with H5Lite" ON)
if(QtSupport_INCLUDE_QT_API)
  set(QtSupport_USE_QT 1)
endif()
MARK_AS_ADVANCED(QtSupport_INCLUDE_QT_API)
MARK_AS_ADVANCED(QtSupport_USE_QT)

set(QtSupport_HDRS
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSFileUtils.h
)

set(QtSupport_MOC_HDRS
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSFileCompleter.h
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSLineEdit.h
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSRecentFileList.h
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSSettings.h
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSStringEdit.h
)

set(QtSupport_SRCS
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSFileCompleter.cpp
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSLineEdit.cpp
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSFileUtils.cpp
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSRecentFileList.cpp
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSSettings.cpp
  ${EMsoft_SOURCE_DIR}/Source/QtSupport/QtSStringEdit.cpp
)

# --------------------------------------------------------------------
# Wrap UI files so they are AUTO UIC'ed
QT5_WRAP_UI( SVWidgetsLib_QtSupport_Generated_UI_HDRS
${EMsoft_SOURCE_DIR}/Source/QtSupport/UI_Files/QtSStringEdit.ui
)

#---------------------------------
# Now configure the main header file.
cmp_IDE_SOURCE_PROPERTIES( "" "${QtSupport_HDRS}" "${QtSupport_SRCS}" ${PROJECT_INSTALL_HEADERS})
cmp_IDE_SOURCE_PROPERTIES( "Generated/Qt_Uic" "${SVWidgetsLib_QtSupport_Generated_UI_HDRS}" "" "0")

# --------------------------------------------------------------------
# Generate a Header file with Compile Version variables
# --------------------------------------------------------------------
# set(VERSION_GEN_NAME "QtSupport")
# set(VERSION_GEN_NAMESPACE "QtSupport")
# set(PROJECT_PREFIX "QtSupport")
# set(VERSION_GEN_COMPLETE  "1.0.0")
# set(VERSION_GEN_VER_MAJOR "1")
# set(VERSION_GEN_VER_MINOR "0")
# set(VERSION_GEN_VER_PATCH "0")
# set(VERSION_GEN_HEADER_FILE_NAME "QtSupportVersion.h")

# configure_file(${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersion.h.in
#                ${QtSupport_BINARY_DIR}/${PROJECT_NAME}/QtSupportVersion.h  )
# configure_file(${CMP_CONFIGURED_FILES_SOURCE_DIR}/cmpVersion.cpp.in
#                ${QtSupport_BINARY_DIR}/${PROJECT_NAME}/QtSupportVersion.cpp  )


set(PROJECT_SRCS
    ${QtSupport_SRCS}
    ${QtSupport_MOC_HDRS}
    ${QtSupport_HDRS}
    ${SVWidgetsLib_QtSupport_Generated_UI_HDRS}
)

add_library(${PROJECT_NAME} ${LIB_TYPE} ${PROJECT_SRCS})
CMP_AddDefinitions(TARGET ${PROJECT_NAME})
LibraryProperties( ${PROJECT_NAME}  ${EXE_DEBUG_EXTENSION} )
CMP_MODULE_INCLUDE_DIRS (TARGET ${PROJECT_NAME} LIBVARS HDF5 Qt5Core)

get_filename_component(TARGET_BINARY_DIR_PARENT ${QtSupport_BINARY_DIR} PATH)
get_filename_component(TARGET_SOURCE_DIR_PARENT ${QtSupport_SOURCE_DIR} PATH)

set(lib_type "static")
if(BUILD_SHARED_LIBS)
  set(lib_type "shared")
endif()
set(hdf5LinkLibs hdf5 hdf5_f90cstub hdf5_fortran hdf5_hl hdf5_hl_f90cstub hdf5_hl_fortran)
set(comps ${hdf5LinkLibs})
set(hdf5LinkLibs "")
foreach(comp ${comps})
  if(TARGET hdf5::${comp}-${lib_type})
    set(hdf5LinkLibs ${hdf5LinkLibs} hdf5::${comp}-${lib_type})
  endif() 
endforeach()


TARGET_INCLUDE_DIRECTORIES( ${PROJECT_NAME} PUBLIC
                              $<INSTALL_INTERFACE:${INCLUDE_INSTALL_DIR}>
                              $<BUILD_INTERFACE:${TARGET_SOURCE_DIR_PARENT}>
                              $<BUILD_INTERFACE:${TARGET_BINARY_DIR_PARENT}>
                              ${EMsoftWorkbench_SOURCE_DIR}/Source/Common
                              ${EMsoftWorkbench_BINARY_DIR}
                              )

if(BUILD_SHARED_LIBS)
  if(WIN32)
    target_compile_definitions(${PROJECT_NAME} PUBLIC "-DQtSupport_BUILT_AS_DYNAMIC_LIB")
  endif(WIN32)
endif(BUILD_SHARED_LIBS)

target_link_libraries(${PROJECT_NAME} ${hdf5LinkLibs} Qt5::Core )


set(install_dir "bin")
set(lib_install_dir "lib")

if(APPLE)
  get_property(DREAM3D_PACKAGE_DEST_PREFIX GLOBAL PROPERTY DREAM3D_PACKAGE_DEST_PREFIX)
  set(install_dir "${DREAM3D_PACKAGE_DEST_PREFIX}bin")
  set(lib_install_dir "${DREAM3D_PACKAGE_DEST_PREFIX}lib")
elseif(WIN32)
  set(install_dir ".")
  set(lib_install_dir ".")
endif()

if (QtSupport_INSTALL_FILES)

  INSTALL(TARGETS ${PROJECT_NAME}
      EXPORT QtSupportTargets
      COMPONENT Applications
      RUNTIME DESTINATION ${install_dir}
      LIBRARY DESTINATION ${lib_install_dir}
      ARCHIVE DESTINATION lib
      BUNDLE DESTINATION "."
  )


  # --------------------------------------------------------------------
  # Allow the generation and installation of a CMake configuration file
  # which makes using EMsoft from another project easier.
  # --------------------------------------------------------------------
  export(EXPORT QtSupportTargets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}/${PROJECT_NAME}Targets.cmake"
    NAMESPACE QtSupport::
  )


  install(EXPORT QtSupportTargets
    FILE
      ${PROJECT_NAME}Targets.cmake
    NAMESPACE
      ${PROJECT_NAME}::
    DESTINATION
      ${ConfigPackageLocation}
  )

  install(FILES 
      ${CMP_HEADER_DIR}/${CMP_TOP_HEADER_FILE}
      ${CMP_HEADER_DIR}/${CMP_TYPES_FILE_NAME}
      ${CMP_HEADER_DIR}/${CMP_CONFIGURATION_FILE_NAME}
      ${CMP_HEADER_DIR}/${CMP_VERSION_HEADER_FILE_NAME}
    DESTINATION
      include/${PROJECT_NAME}
  )

endif()


# ------- Enable the CTest testing. ---------------
# -------------------------------------------------------------------- 
# If Testing is enabled, turn on the Unit Tests 
if(EMsoft_BUILD_TESTING)
    include(${QtSupport_SOURCE_DIR}/Test/CMakeLists.txt)
endif()


