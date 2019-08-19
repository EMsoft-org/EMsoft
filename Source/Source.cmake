

set_property(GLOBAL PROPERTY EMsoft_PACKAGE_DEST_PREFIX ".")
set(EMsoftWorkbench_APPLICATION_NAME "")
# -----------------------------------------------------------------------
# 
# -----------------------------------------------------------------------
option(EMsoft_ENABLE_EMsoftWorkbench "Build_EMsoftWorkbench" OFF)
if( EMsoft_ENABLE_EMsoftWorkbench AND APPLE)
  set(EMsoftWorkbench_APPLICATION_NAME "EMsoftWorkbench")
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set_property(GLOBAL PROPERTY EMsoft_PACKAGE_DEST_PREFIX "${EMsoftWorkbench_APPLICATION_NAME}${EXE_DEBUG_EXTENSION}.app/Contents")
  else()
    set_property(GLOBAL PROPERTY EMsoft_PACKAGE_DEST_PREFIX "${EMsoftWorkbench_APPLICATION_NAME}.app/Contents")
  endif()
endif()

get_property(EMsoft_PACKAGE_DEST_PREFIX GLOBAL PROPERTY EMsoft_PACKAGE_DEST_PREFIX)

include("${EMsoft_SOURCE_DIR}/Source/EMsoft_Functions.cmake")

add_subdirectory(${PROJECT_SOURCE_DIR}/Source/EMsoftLib ${PROJECT_BINARY_DIR}/EMsoftLib)

option(EMsoft_ENABLE_HDF5_SUPPORT "Enable HDF5 based I/O" ON)
if( ${EMsoft_ENABLE_HDF5_SUPPORT} )
  add_subdirectory(${PROJECT_SOURCE_DIR}/Source/EMsoftHDFLib ${PROJECT_BINARY_DIR}/EMsoftHDFLib)
endif()

# if the EMSphInx folder exists, then we include it in the build
if (EXISTS ${PROJECT_SOURCE_DIR}/Source/EMSphInx)
  add_subdirectory(${PROJECT_SOURCE_DIR}/Source/EMSphInx/f90/EMSphInxLib ${PROJECT_BINARY_DIR}/EMsoftSphInxLib)
  set(MODALITY_DIRS
    DictionaryIndexing
    OLIO
    OM
    SEM
    EMSphInx/f90/EMSphInxSrc
    TEM
    QC
    Utilities
    XRay
  )
else()
  set(MODALITY_DIRS
    DictionaryIndexing
    OLIO
    OM
    SEM
    TEM
    QC
    Utilities
    XRay
  )
endif()

# -----------------------------------------------------------------------
# Establish which modalities are going to be compiled
# -----------------------------------------------------------------------
foreach(MODALITY ${MODALITY_DIRS})
  option(EMsoft_ENABLE_${MODALITY} "Build sources and programs related to ${MODALITY}" ON)
endforeach()


# -----------------------------------------------------------------------
# Add a wrapper lib thats uses the enabled modality options to compile itself
# -----------------------------------------------------------------------
add_subdirectory(${PROJECT_SOURCE_DIR}/Source/EMsoftWrapperLib ${PROJECT_BINARY_DIR}/EMsoftWrapperLib)

# -----------------------------------------------------------------------
# Add the executables
# -----------------------------------------------------------------------
foreach(MODALITY ${MODALITY_DIRS})
  if( "${EMsoft_ENABLE_${MODALITY}}" STREQUAL "ON" )
    message(STATUS "EMsoft: Enabling public ${MODALITY} Modality")
    add_subdirectory( ${PROJECT_SOURCE_DIR}/Source/${MODALITY} ${PROJECT_BINARY_DIR}/${MODALITY})
  endif()
endforeach()



# -----------------------------------------------------------------------
# Does the developer want to compile the GUI for EMsoft?
# -----------------------------------------------------------------------
if( EMsoft_ENABLE_EMsoftWorkbench )

  INCLUDE (${EMsoft_SOURCE_DIR}/Support/cmp/cmpCMakeMacros.cmake )
  # --------------------------------------------------------------------
  # Find and Use the Qt5 Libraries
  include(${EMsoft_SOURCE_DIR}/Support/cmp/ExtLib/Qt5Support.cmake)
  set(EMsoftWorkbench_Qt5_Components Core Widgets Network Gui Concurrent Svg Xml OpenGL PrintSupport )
  CMP_AddQt5Support( "${EMsoftWorkbench_Qt5_Components}"
                    "FALSE"
                    "${EMsoft_BINARY_DIR}"
                    "EMsoftWorkbench")
  
  include(${PROJECT_SOURCE_DIR}/Source/EMsoftWorkbench/SourceList.cmake)
endif()

