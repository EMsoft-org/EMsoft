
PROJECT( EMsoftWorkbench VERSION ${EMsoft_VERSION_MAJOR}.${EMsoft_VERSION_MINOR}.${EMsoft_VERSION_PATCH}.0)

set(CMP_HEADER_DIR ${EMsoftWorkbench_BINARY_DIR})
set(CMP_CONFIGURATION_FILE_NAME "EMsoftWorkbenchConfiguration.h")
set(CMP_TYPES_FILE_NAME "EMsoftWorkbenchTypes.h")
set(CMP_VERSION_HEADER_FILE_NAME "EMsoftWorkbenchVersion.h")
set(CMP_VERSION_SOURCE_FILE_NAME "EMsoftWorkbenchVersion.cpp")
set(CMP_TOP_HEADER_FILE "EMsoftWorkbench.h")
set(CMP_PROJECT_NAMESPACE "${PROJECT_NAME}")
set(CMP_PROJECT_NAME "${PROJECT_NAME}")
set(CMP_ENABLE_PLUGINS "0")
set(CMP_LIB_SEARCH_DIRS "")
set(CMP_GENERATE_VERSION_STRING 1)
set(PROJECT_INSTALL_HEADERS "0")
set(PROJECT_RESOURCES_DIR "${EMsoftWorkbench_SOURCE_DIR}/Resources")
set(EXE_DEBUG_EXTENSION "_debug")


#Enable use of Solution Folders
set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(PROJECT_INSTALL_HEADERS "0")
set(PROJECT_INSTALL_EXECUTABLES "1")

#------------------------------------------------------------------------------
# Find the Git Package for Versioning. It should be ok if Git is NOT found
Find_package(Git)
# -----------------------------------------------------------------------
# Generate a set of Version Cpp/H files that will be used by SIMPLView to
# get and display the proper version numbers.
cmpRevisionString( GENERATED_HEADER_FILE_PATH "${EMsoftWorkbench_VERSION_HDR_FILE}"
                        GENERATED_SOURCE_FILE_PATH "${EMsoftWorkbench_VERSION_SRC_FILE}"
                        NAMESPACE "EMsoftWorkbench"
                        PROJECT_NAME "${PROJECT_NAME}"
                        EXPORT_MACRO "EMsoftWorkbench_EXPORT")

include(${CMP_SOURCE_DIR}/cmpProject.cmake)

# --------------------------------------------------------------------
# Find and Use the Qt5 Libraries
include(${CMP_SOURCE_DIR}/ExtLib/Qt5Support.cmake)
set(EMsoftWorkbench_Qt5_Components Core Widgets Network Gui Concurrent Svg Xml OpenGL PrintSupport )
CMP_AddQt5Support( "${EMsoftWorkbench_Qt5_Components}"
                    "FALSE"
                    "${EMsoftWorkbenchProj_BINARY_DIR}"
                    "EMsoftWorkbench")

# --------------------------------------------------------------------
# Look for Qt 5 as we need it for the plugin GUI to be generated
if(NOT Qt5Core_FOUND)
    message(FATAL_ERROR "Qt 5 is Needed for this project")
endif()

# --------------------------------------------------------------------
# EMsoftWorkbench needs the Eigen library
set(EMsoftWorkbench_USE_EIGEN "")
Find_Package(Eigen3 REQUIRED)
if(EIGEN3_FOUND)
  message(STATUS "Eigen3 Location: ${EIGEN3_ROOT_DIR}")
  message(STATUS "Eigen3 Version: ${EIGEN3_VERSION_STRING}")
  set(EMsoftWorkbench_USE_EIGEN "1")
else()
  message(WARNING "The Eigen Library is required for some algorithms to execute. These algorithms will be disabled.")
endif()

# Set the GUI type for different platforms
set(GUI_TYPE )

# Set some Win32 Specific Settings
if(WIN32)
 set(GUI_TYPE WIN32)
ENDif(WIN32)

find_package(OpenGL)
if(NOT OPENGL_FOUND)
    message(FATAL_ERROR "OpenGL is Needed for this project")
endif()

# and finally any resource files
set(EMsoftWorkbench_RCS
    ${PROJECT_RESOURCES_DIR}/icons/images/Icons.qrc
    ${PROJECT_RESOURCES_DIR}/icons/splash/splash.qrc
    ${PROJECT_RESOURCES_DIR}/icons/gifs/gifs.qrc
    ${PROJECT_RESOURCES_DIR}/StyleSheets/StyleSheets.qrc
    ${PROJECT_RESOURCES_DIR}/Images/Images.qrc
    ${PROJECT_RESOURCES_DIR}/Fonts/Fonts.qrc
)
cmp_IDE_SOURCE_PROPERTIES( "EMsoftWorkbench/Qrc" "${EMsoftWorkbench_RCS}" "" "${PROJECT_INSTALL_HEADERS}")

# -- Run MOC and UIC on the necessary files
QT5_ADD_RESOURCES( EMsoftWorkbench_Generated_RC_SRCS 
    ${EMsoftWorkbench_RCS} ${QRC_LICENSE_FILES}
    )
cmp_IDE_GENERATED_PROPERTIES( "Generated/Qt_Rc" "" "${EMsoftWorkbench_Generated_RC_SRCS}" "0")

include(${EMsoftWorkbench_SOURCE_DIR}/Common/SourceList.cmake)
include(${EMsoftWorkbench_SOURCE_DIR}/Modules/SourceList.cmake)
include(${EMsoftWorkbench_SOURCE_DIR}/QtSupport/SourceList.cmake)

# --------------------------------------------------------------------
# Any Class that inherits from QObject, either directly or through the heirarchy needs to have its header listed here
set(EMsoftWorkbench_Moc_HDRS
  ${EMsoftWorkbench_SOURCE_DIR}/EMsoftApplication.h
  ${EMsoftWorkbench_SOURCE_DIR}/EMsoftWorkbench_UI.h
  ${EMsoftWorkbench_SOURCE_DIR}/StatusBarWidget.h
  ${EMsoftWorkbench_SOURCE_DIR}/StyleSheetEditor.h
  ${EMsoftWorkbench_SOURCE_DIR}/SVStyle.h
)

# --------------------------------------------------------------------
# Run Qts automoc program to generate some source files that get compiled
QT5_WRAP_CPP( EMsoftWorkbench_Generated_MOC_SRCS ${EMsoftWorkbench_Moc_HDRS})
set_source_files_properties( ${EMsoftWorkbench_Generated_MOC_SRCS} PROPERTIES GENERATED TRUE)
#set_source_files_properties( ${EMsoftWorkbench_Generated_MOC_SRCS} PROPERTIES HEADER_FILE_ONLY TRUE)

set(EMsoftWorkbench_HDRS
  
)

set(EMsoftWorkbench_SRCS
  ${EMsoftWorkbench_SOURCE_DIR}/EMsoftApplication.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/EMsoftWorkbench_UI.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/main.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/StatusBarWidget.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/StyleSheetEditor.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/SVStyle.cpp
)

set(EMsoftWorkbench_UIS
  ${EMsoftWorkbench_SOURCE_DIR}/UI_Files/EMsoftWorkbench_UI.ui
  ${EMsoftWorkbench_SOURCE_DIR}/UI_Files/StatusBarWidget.ui
  ${EMsoftWorkbench_SOURCE_DIR}/UI_Files/StyleSheetEditor.ui
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/UI_Files/QtSStringEdit.ui
  )
# --------------------------------------------------------------------
# Continue on with our Qt4 section
QT5_WRAP_UI( EMsoftWorkbench_Generated_UI_HDRS    ${EMsoftWorkbench_UIS})
cmp_IDE_SOURCE_PROPERTIES( "EMsoftWorkbench/UI_Files" "${EMsoftWorkbench_UIS}" "" "${PROJECT_INSTALL_HEADERS}")
cmp_IDE_SOURCE_PROPERTIES( "EMsoftWorkbench" "${EMsoftWorkbench_HDRS};${EMsoftWorkbench_Moc_HDRS}" "${EMsoftWorkbench_SRCS}" "${PROJECT_INSTALL_HEADERS}")
cmp_IDE_GENERATED_PROPERTIES( "Generated/EMsoftWorkbench" "" "${EMsoftWorkbench_Generated_MOC_SRCS}" "0")
cmp_IDE_GENERATED_PROPERTIES( "Generated/Qt_Uic" "${EMsoftWorkbench_Generated_UI_HDRS}" "" "0")


set(EMsoftWorkbench_PROJECT_SRCS
    ${EMsoftWorkbench_Moc_HDRS}
    ${EMsoftWorkbench_HDRS}
    ${EMsoftWorkbench_SRCS}
    ${EMsoftWorkbench_Generated_MOC_SRCS}
    ${EMsoftWorkbench_Generated_UI_HDRS}

    ${EMsoftWorkbench_Generated_RC_SRCS}

    ${EMsoftWorkbench_Common_SOURCES}
    ${EMsoftWorkbench_SOURCES}
    ${EMsoftWorkbench_Modules_SOURCES}
    ${EMsoftWorkbench_PatternDisplayModule_SOURCES}
    ${EMsoftWorkbench_PatternDisplayModule_AngleWidgets_SOURCES}
    ${EMsoftWorkbench_PatternFitModule_SOURCES}
    ${EMsoftWorkbench_CrystalStructureCreationModule_SOURCES}
    ${EMsoftWorkbench_MonteCarloSimulationModule_SOURCES}
    ${EMsoftWorkbench_MasterPatternSimulationModule_SOURCES}
    ${EMsoftWorkbench_DictionaryIndexingModule_SOURCES}
  
    ${QtSupport_SRCS}
    ${QtSupport_MOC_HDRS}
    ${QtSupport_HDRS}
    ${SVWidgetsLib_QtSupport_Generated_UI_HDRS}
    ${QtSupport_MOC_HDRS_Generated_MOC_SRCS}

)

if(APPLE)
    set(APPLICATION_ICON ${PROJECT_RESOURCES_DIR}/icons/icns/${PROJECT_NAME}.icns)
endif(APPLE)

if(WIN32)
    set(APPLICATION_ICON ${PROJECT_RESOURCES_DIR}/icons/ico/${PROJECT_NAME}.ico)
ENDif(WIN32)

set(DEST_DIR ".")
if(NOT APPLE)
    set(DEST_DIR "bin")
endif()

  
GetHDF5LinkLibraries(EMSOFT)
set(EXE_LINK_LIBRARIES 
  ${EXE_LINK_LIBRARIES} 
  ${EMSOFT_hdf5LinkLibs} 
  EMsoftHDFLib 
  EMsoftLib 
  EMsoftWrapperLib 
  H5Support
  OrientationLib
  )


if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  set(EXE_LINK_LIBRARIES ${EXE_LINK_LIBRARIES} gomp gcc_eh)
endif()

BuildQtAppBundle(
    TARGET ${PROJECT_NAME}
    SOURCES ${EMsoftWorkbench_PROJECT_SRCS}
    DEBUG_EXTENSION ${EXE_DEBUG_EXTENSION}
    ICON_FILE  ${APPLICATION_ICON}
    VERSION_MAJOR ${EMsoft_VERSION_MAJOR}
    VERSION_MINOR ${EMsoft_VERSION_MINOR}
    VERSION_PATCH ${EMsoft_VERSION_PATCH}
    BINARY_DIR    ${${PROJECT_NAME}_BINARY_DIR}
    LINK_LIBRARIES ${OPENGL_LIBRARIES} ${EXE_LINK_LIBRARIES} 
    LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    QT_PLUGINS    ${QT_PLUGINS}
    QT5_MODULES   Core Gui Widgets OpenGL Network Concurrent
    COMPONENT     Applications
    INSTALL_DEST  ${DEST_DIR}
)
set_target_properties (${PROJECT_NAME} PROPERTIES
  FOLDER ${PROJECT_NAME}
)

target_include_directories(${PROJECT_NAME} 
  PUBLIC 
    ${EMsoftWorkbench_SOURCE_DIR} 
    ${EMsoftWorkbench_SOURCE_DIR}/Common 
    ${EMsoftWorkbench_SOURCE_DIR}/QtSupport
    ${EMsoft_SOURCE_DIR}/Source
    ${EMsoftWorkbench_BINARY_DIR}
    ${EIGEN3_INCLUDE_DIRS}
    )

# #- This installs all the help files. Note that we assume a flat directory structure
# #- in the "Help" folder, ie, all html files, images, etc are all in the "Help"
# #- folder and NOT in any subfolders beyond that.
# install(DIRECTORY ${EMsoftWorkbench_SOURCE_DIR}/Documentation
#         DESTINATION .
#         COMPONENT Applications )

