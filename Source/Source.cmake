
option(EMsoft_ENABLE_HDF5_SUPPORT "Enable HDF5 based I/O" ON)

add_subdirectory(${PROJECT_SOURCE_DIR}/Source/EMsoftLib ${PROJECT_BINARY_DIR}/EMsoftLib)


if( ${EMsoft_ENABLE_HDF5_SUPPORT} )
  add_subdirectory(${PROJECT_SOURCE_DIR}/Source/EMsoftHDFLib ${PROJECT_BINARY_DIR}/EMsoftHDFLib)
endif()

set(MODALITY_DIRS
  DictionaryIndexing
  OM
  SEM
  TEM
  Utilities
)

# --------------------------------------------------------------------
# Add the executables
foreach(MODALITY ${MODALITY_DIRS})

  option(EMsoft_ENABLE_${MODALITY} "Build sources and programs related to ${MODALITY}" ON)
  if( "${EMsoft_ENABLE_${MODALITY}}" STREQUAL "ON" )
    message(STATUS "EMsoft: Enabling public ${MODALITY} Modality")
    add_subdirectory( ${PROJECT_SOURCE_DIR}/Source/${MODALITY} ${PROJECT_BINARY_DIR}/${MODALITY})
  endif()

endforeach()



# -----------------------------------------------------------------------
# 
# -----------------------------------------------------------------------
option(EMsoft_ENABLE_EMsoftWorkbench "Build_EMsoftWorkbench" OFF)
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

  add_subdirectory( ${PROJECT_SOURCE_DIR}/Source/H5Support ${PROJECT_BINARY_DIR}/H5Support)

  add_subdirectory( ${PROJECT_SOURCE_DIR}/Source/OrientationLib ${PROJECT_BINARY_DIR}/OrientationLib)

  add_subdirectory( ${PROJECT_SOURCE_DIR}/Source/SIMPLib ${PROJECT_BINARY_DIR}/SIMPLib)

  add_subdirectory(${PROJECT_SOURCE_DIR}/Source/EMsoftWorkbench ${PROJECT_BINARY_DIR}/EMsoftWorkbench)
endif()

