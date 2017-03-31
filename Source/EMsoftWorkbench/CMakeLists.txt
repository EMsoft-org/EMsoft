
PROJECT( EMsoftWorkbench VERSION 0.1.1)

set(ProjectBinaryDir ${EMsoftWorkbench_BINARY_DIR})

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
Find_Package(Eigen REQUIRED)
if(EIGEN_FOUND)
  message(STATUS "Found Eigen Library")
  include_directories( ${EIGEN_INCLUDE_DIRS} )
  include_directories( ${EIGEN_INCLUDE_DIR} )
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

set(EMsoftWorkbench_SRCS
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/EMsoftApplication.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/EMsoftController.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/EMsoftMenuItems.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/EMsoftWorkbench.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/GLImageDisplayWidget.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/GLImageViewer.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/LandingWidget.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/LandingWidgetListItem.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/LandingWidgetListItemDelegate.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/LandingWidgetListModel.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/main.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/MPMCDisplayWidget.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/PatternDisplayWidget.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/PatternListItem.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/PatternListItemDelegate.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/PatternListModel.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/QtSRecentFileList.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/QtSSettings.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/StandardEMsoftApplication.cpp
  )


set(EMsoftWorkbench_HDRS
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/Constants.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/LandingWidgetListItem.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/PatternListItem.h
  )

set(EMsoftWorkbench_MOC_HDRS
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/EMsoftApplication.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/EMsoftController.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/EMsoftMenuItems.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/EMsoftWorkbench.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/GLImageDisplayWidget.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/GLImageViewer.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/LandingWidget.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/LandingWidgetListItemDelegate.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/LandingWidgetListModel.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/MPMCDisplayWidget.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/PatternDisplayWidget.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/PatternListItemDelegate.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/PatternListModel.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/ProjectionConversions.hpp
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/QtSRecentFileList.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/QtSSettings.h
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/StandardEMsoftApplication.h
  )


if(APPLE)
  set(EMsoftWorkbench_SRCS 
    ${EMsoftWorkbench_SRCS}
    ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/MacEMsoftApplication.cpp
    )
  set(EMsoftWorkbench_MOC_HDRS
      ${EMsoftWorkbench_MOC_HDRS}
      ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/MacEMsoftApplication.h
    )
endif()


cmp_IDE_SOURCE_PROPERTIES( "EMsoftWorkbench" "${EMsoftWorkbench_HDRS};${EMsoftWorkbench_MOC_HDRS}" "${EMsoftWorkbench_SRCS}" "${PROJECT_INSTALL_HEADERS}")

# The UI files As generated by Qt Designer
set(EMsoftWorkbench_UIS
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/UI_Files/EMsoftWorkbench.ui
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/UI_Files/LandingWidget.ui
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/UI_Files/MPMCDisplayWidget.ui
  ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/UI_Files/PatternDisplayWidget.ui
)

cmp_IDE_GENERATED_PROPERTIES("EMsoftWorkbench/UI_Files" "${EMsoftWorkbench_UIS}" "")

# and finally any resource files
set(EMsoftWorkbench_RCS
    ${PROJECT_RESOURCES_DIR}/icons/images/Icons.qrc
    ${PROJECT_RESOURCES_DIR}/icons/splash/splash.qrc
    ${PROJECT_RESOURCES_DIR}/icons/gifs/gifs.qrc
)

# -- Run MOC and UIC on the necessary files
QT5_ADD_RESOURCES( EMsoftWorkbench_Generated_RC_SRCS ${EMsoftWorkbench_RCS} ${QRC_LICENSE_FILES})

# this will run uic on .ui files:
QT5_WRAP_UI( EMsoftWorkbench_Generated_UI_HDRS ${EMsoftWorkbench_UIS} )

# and finally this will run moc:
QT5_WRAP_CPP( EMsoftWorkbench_Generated_MOC_SRCS ${EMsoftWorkbench_MOC_HDRS} )

#-- Put the generated files into their own group for IDEs
cmp_IDE_GENERATED_PROPERTIES("Generated/ui" "${EMsoftWorkbench_Generated_UI_HDRS}" "")
cmp_IDE_GENERATED_PROPERTIES("Generated/moc" "${EMsoftWorkbench_Generated_MOC_SRCS}" "")
cmp_IDE_GENERATED_PROPERTIES("Generated/qrc" "${QRC_LICENSE_FILES}" "${EMsoftWorkbench_Generated_RC_SRCS}")

include(${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench/AngleWidgets/SourceList.cmake)

# -- Finally set the Project Sources to include all the sources
set(${PROJECT_NAME}_PROJECT_SRCS
  ${EMsoftWorkbench_SRCS}
  ${EMsoftWorkbench_HDRS}
  ${EMsoftWorkbench_MOC_HDRS}
  ${EMsoftWorkbench_Generated_MOC_SRCS}
  ${EMsoftWorkbench_Generated_RC_SRCS}
  ${EMsoftWorkbench_Generated_UI_HDRS}
  ${Angle_Widgets_HDRS}
  ${Angle_Widgets_MOC_HDRS}
  ${Angle_Widgets_SRCS}
  ${Angle_Widgets_Generated_MOC_SRCS}
  ${Angle_Widgets_Generated_UI_HDRS}
  )

include_directories(${HDF5_INCLUDE_DIR})

include_directories(${EMsoftWorkbench_SOURCE_DIR}/Source )
include_directories(${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench )
include_directories(${EMsoftWorkbench_BINARY_DIR})
include_directories(${EMsoft_SOURCE_DIR}/Source)
include_directories(${EMsoft_BINARY_DIR})



if(APPLE)
    set(APPLICATION_ICON ${PROJECT_RESOURCES_DIR}/icons/icns/${PROJECT_NAME}.icns)
endif(APPLE)

if(WIN32)
    set(APPLICATION_ICON ${PROJECT_RESOURCES_DIR}/icons/ico/${PROJECT_NAME}.ico)
ENDif(WIN32)

set(DEST_DIR ".")
if(UNIX AND NOT APPLE)
    set(DEST_DIR "bin")
endif()

  
string(TOLOWER ${LIB_TYPE} lib_type)
set(hdf5LinkLibs hdf5)
set(comps ${hdf5LinkLibs})
if (HDF5_VERSION_STRING VERSION_GREATER 1.8.15)
  set(hdf5LinkLibs "")
  foreach(comp ${comps})
    if(TARGET ${comp}-${lib_type})
      set(hdf5LinkLibs ${hdf5LinkLibs} ${comp}-${lib_type})
    endif()
  endforeach()
endif()

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  set(FORTRAN_LIBRARIES gomp gcc_eh)
endif()


BuildQtAppBundle(
    TARGET ${PROJECT_NAME}
    SOURCES ${EMsoftWorkbench_PROJECT_SRCS}
    DEBUG_EXTENSION ${EXE_DEBUG_EXTENSION}
    ICON_FILE  ${APPLICATION_ICON}
    VERSION_MAJOR ${EMsoftWorkbench_VER_MAJOR}
    VERSION_MINOR ${EMsoftWorkbench_VER_MINOR}
    VERSION_PATCH ${EMsoftWorkbench_VER_PATCH}
    BINARY_DIR    ${${PROJECT_NAME}_BINARY_DIR}
    LINK_LIBRARIES ${OPENGL_LIBRARIES} ${hdf5LinkLibs} ${FORTRAN_LIBRARIES} H5Support OrientationLib EMsoftLib
    LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    QT5_MODULES   Core Gui Widgets OpenGL
    COMPONENT     Applications
    INSTALL_DEST  ${DEST_DIR}
)
set_target_properties (${PROJECT_NAME} PROPERTIES
  FOLDER Source/${PROJECT_NAME}
)


#- This installs all the help files. Note that we assume a flat directory structure
#- in the "Help" folder, ie, all html files, images, etc are all in the "Help"
#- folder and NOT in any subfolders beyond that.
install(DIRECTORY ${EMsoftWorkbench_SOURCE_DIR}/Template
        DESTINATION .
        COMPONENT Applications )
