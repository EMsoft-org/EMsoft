
# set project's name
PROJECT( SIMPLib VERSION ${EMsoft_VERSION_MAJOR}.${EMsoft_VERSION_MINOR})

set(PROJECT_PREFIX "SIMPLib" CACHE STRING "The Prefix to be used for Preprocessor definitions")
# -----------------------------------------------------------------------
# The project generally does NOT want to install headers. See if another
# project has over ridden that property
get_property(CMP_PROJECT_INSTALL_HEADERS GLOBAL PROPERTY CMP_PROJECT_INSTALL_HEADERS)
if("${CMP_PROJECT_INSTALL_HEADERS}" STREQUAL "")
  set_property(GLOBAL PROPERTY CMP_PROJECT_INSTALL_HEADERS "1")
  set(PROJECT_INSTALL_HEADERS "1")
else()
  set(PROJECT_INSTALL_HEADERS "${CMP_PROJECT_INSTALL_HEADERS}")
endif()

#-------------------------------------------------------------------------------
# Include the cmp project to perform all the basic configuration tests for the
# project.
#-------------------------------------------------------------------------------
set(CMP_HEADER_DIR ${SIMPLib_BINARY_DIR})
set(CMP_CONFIGURATION_FILE_NAME "SIMPLibConfiguration.h")
set(CMP_TYPES_FILE_NAME "SIMPLibTypes.h")
set(CMP_VERSION_HEADER_FILE_NAME "SIMPLibVersion.h")
set(CMP_VERSION_SOURCE_FILE_NAME "SIMPLibVersion.cpp")
set(CMP_TOP_HEADER_FILE "SIMPLib.h")
set(CMP_PROJECT_NAMESPACE "${PROJECT_NAME}")
set(CMP_PROJECT_NAME "${PROJECT_NAME}")
set(CMP_ENABLE_PLUGINS "0")
set(CMP_LIB_SEARCH_DIRS "")
set(CMP_GENERATE_VERSION_STRING 1)
set(PROJECT_INSTALL_HEADERS 0)
set(EXE_DEBUG_EXTENSION "_debug")
set(SIMPLib_FILTER_DOC_DIR ${SIMPLib_SOURCE_DIR}/Documentation/SIMPLibFilters)

include(GenerateExportHeader)


cmpConfigureFileWithMD5Check(CONFIGURED_TEMPLATE_PATH ${SIMPLib_SOURCE_DIR}/SIMPLibConfiguration.h.in
                            GENERATED_FILE_PATH ${SIMPLib_BINARY_DIR}/${CMP_TOP_HEADER_FILE})

cmp_IDE_GENERATED_PROPERTIES("Generated"
                             "${CMP_HEADER_DIR}/${CMP_TOP_HEADER_FILE};${CMP_HEADER_DIR}/${CMP_TYPES_FILE_NAME};${CMP_HEADER_DIR}/${CMP_CONFIGURATION_FILE_NAME};${CMP_HEADER_DIR}/${CMP_VERSION_HEADER_FILE_NAME}"
                             "${CMP_HEADER_DIR}/${CMP_VERSION_SOURCE_FILE_NAME}")

set(Project_SRCS
    ${CMP_HEADER_DIR}/${CMP_TOP_HEADER_FILE}
    ${CMP_HEADER_DIR}/${CMP_TYPES_FILE_NAME}
    ${CMP_HEADER_DIR}/${CMP_CONFIGURATION_FILE_NAME}
    ${CMP_HEADER_DIR}/${CMP_VERSION_HEADER_FILE_NAME}
    ${CMP_HEADER_DIR}/${CMP_VERSION_SOURCE_FILE_NAME}
)

#----- Bring in our CMake file with the macros
include(${SIMPLib_SOURCE_DIR}/SIMPLibMacros.cmake)


#-------------------------------------------------------------------------------
macro(D3DCompileDir DirName)
    set(SIMPLib_${DirName}_Generated_MOC_SRCS "")
    include(${SIMPLib_SOURCE_DIR}/${DirName}/SourceList.cmake)

    foreach(f ${SIMPLib_${DirName}_Generated_MOC_SRCS})
        set_source_files_properties( ${f} PROPERTIES GENERATED TRUE)
        set_source_files_properties( ${f} PROPERTIES HEADER_FILE_ONLY TRUE)
    endforeach()
    set(Project_SRCS ${Project_SRCS} ${SIMPLib_${DirName}_SRCS} ${SIMPLib_${DirName}_HDRS} ${SIMPLib_${SUBDIR_NAME}_Generated_MOC_SRCS})
endMacro()

# Add in our NON-Filter classes
D3DCompileDir(Common)
# D3DCompileDir(DataArrays)
# D3DCompileDir(DataContainers)
# D3DCompileDir(Geometry)
# D3DCompileDir(FilterParameters)
# D3DCompileDir(HDF5)
# D3DCompileDir(Math)
# D3DCompileDir(Plugin)
# D3DCompileDir(StatsData)
D3DCompileDir(Utilities)
# D3DCompileDir(VTKUtils)


#--------------------------
# include_directories( ${HDF5_INCLUDE_DIR} )
# include_directories( ${PROJECT_CODE_DIR} )
# include_directories( ${SIMPLib_BINARY_DIR} )
# include_directories(${EMsoft_SOURCE_DIR}/Source)
# include_directories(${EMsoft_BINARY_DIR})

set(${PROJECT_NAME}_LINK_LIBS Qt5::Core H5Support)
if( "${SIMPL_USE_MULTITHREADED_ALGOS}" STREQUAL "ON")
  list(APPEND ${PROJECT_NAME}_LINK_LIBS ${TBB_LIBRARIES})
endif()


#-- Add a library for the SIMPLib Code
add_library(${PROJECT_NAME} ${LIB_TYPE} ${Project_SRCS} )
generate_export_header(${PROJECT_NAME}
  EXPORT_MACRO_NAME ${PROJECT_NAME}_EXPORT
  EXPORT_FILE_NAME ${PROJECT_NAME}DLLExport.h
  )

include(${CMP_SOURCE_DIR}/cmpProject.cmake)
target_link_libraries(${PROJECT_NAME} ${${PROJECT_NAME}_LINK_LIBS})
LibraryProperties( ${PROJECT_NAME} ${EXE_DEBUG_EXTENSION} )
set_target_properties (${PROJECT_NAME} PROPERTIES
  FOLDER EMsoftPublic/${PROJECT_NAME}
)

#---------------------------------------------------------------------
# This sets up the two variables install_dir and lib_install_dir
EMsoft_SetupInstallDirs()

#if(BUILD_SHARED_LIBS  AND NOT APPLE)
    INSTALL(TARGETS ${PROJECT_NAME}
        EXPORT ${PROJECT_NAME}Targets
        COMPONENT Applications
        RUNTIME DESTINATION ${install_dir}
        LIBRARY DESTINATION ${lib_install_dir}
        ARCHIVE DESTINATION ${lib_install_dir}
        BUNDLE DESTINATION "."
    )
#endif()



## --------------------------------------------------------------------
## Allow the generation and installation of a CMake configuration file
## which makes using SIMPL from another project easier.
## --------------------------------------------------------------------
#export(EXPORT SIMPLibTargets
#  FILE "${CMAKE_CURRENT_BINARY_DIR}/SIMPL/SIMPLibTargets.cmake"
#  NAMESPACE SIMPL::
#)

#install(EXPORT SIMPLibTargets
#  FILE
#    SIMPLibTargets.cmake
#  NAMESPACE
#    SIMPL::
#  DESTINATION
#    ${ConfigPackageLocation}
#)





