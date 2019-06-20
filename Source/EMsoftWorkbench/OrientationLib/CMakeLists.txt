
# set project's name
PROJECT(OrientationLib VERSION ${EMsoft_VERSION_MAJOR}.${EMsoft_VERSION_MINOR})
set(PROJECT_NAME "OrientationLib")
set(EXE_DEBUG_EXTENSION "_debug")
set(PROJECT_INSTALL_HEADERS "0")
set(PROJECT_INSTALL_EXECUTABLES "1")

# On mac systems, we likely have to link against the vecLib framework
if(APPLE)
 find_library(LAPACK_LIB Accelerate)
 include_directories("-FvecLib")
endif(APPLE)

if(NOT PROJECT_RESOURCES_DIR)
  set(PROJECT_RESOURCES_DIR ${OrientationLib_SOURCE_DIR}/Resources)
endif()
if(NOT DEFINED CMP_SOURCE_DIR)
  set(CMP_SOURCE_DIR ${OrientationLib_SOURCE_DIR}/Support/cmp)
endif()

# Bring in our Macros from the CMP project
INCLUDE (${CMP_SOURCE_DIR}/cmpCMakeMacros.cmake )


set(PROJECT_PREFIX "OrientationLib" CACHE STRING "The Prefix to be used for Preprocessor definitions")
set(OrientationLib_INSTALL_FILES 0)
#-------------------------------------------------------------------------------
# Include the cmp project to perform all the basic configuration tests for the
# project.
#-------------------------------------------------------------------------------

set(CMP_HEADER_DIR ${OrientationLib_BINARY_DIR})
set(CMP_CONFIGURATION_FILE_NAME "OrientationLibConfiguration.h")
set(CMP_TYPES_FILE_NAME "OrientationLibTypes.h")
set(CMP_VERSION_HEADER_FILE_NAME "OrientationLibVersion.h")
set(CMP_VERSION_SOURCE_FILE_NAME "OrientationLibVersion.cpp")
set(CMP_TOP_HEADER_FILE "OrientationLib.h")
set(CMP_PROJECT_NAMESPACE "${PROJECT_NAME}")
set(CMP_PROJECT_NAME "${PROJECT_NAME}")
set(CMP_ENABLE_PLUGINS "0")
set(CMP_LIB_SEARCH_DIRS "")
set(GENERATE_GIT_VERSION 1)
set(CMP_GENERATE_VERSION_STRING ${GENERATE_GIT_VERSION})
set(PROJECT_INSTALL_HEADERS 0)
set(EXE_DEBUG_EXTENSION "_debug")

include (${CMP_SOURCE_DIR}/cmpProject.cmake)


cmpConfigureFileWithMD5Check(CONFIGURED_TEMPLATE_PATH ${OrientationLib_SOURCE_DIR}/OrientationLibConfiguration.h.in
                            GENERATED_FILE_PATH ${CMP_HEADER_DIR}/${CMP_TOP_HEADER_FILE})


set(OrientationLib_Generated_CMP_SRCS
    ${CMP_HEADER_DIR}/${CMP_VERSION_HEADER_FILE_NAME}
    ${CMP_HEADER_DIR}/${CMP_VERSION_SOURCE_FILE_NAME}
    ${CMP_HEADER_DIR}/${CMP_TOP_HEADER_FILE}
    ${CMP_HEADER_DIR}/${CMP_CONFIGURATION_FILE_NAME}
    ${CMP_HEADER_DIR}/${CMP_TYPES_FILE_NAME}
  )
cmp_IDE_GENERATED_PROPERTIES("Generated" "${OrientationLib_Generated_CMP_SRCS}" "")

# # Setup the Include Directories
# include_directories(${PROJECT_CODE_DIR}
#   ${OrientationLib_BINARY_DIR}
#   ${OrientationLib_BINARY_DIR}/..
#   ${OrientationLib_SOURCE_DIR}/..
#   ${SIMPLProj_SOURCE_DIR}/Source
#   ${SIMPLProj_BINARY_DIR}
#   )

#-- Include all the Source Files
include( ${OrientationLib_SOURCE_DIR}/Utilities/SourceList.cmake)
include( ${OrientationLib_SOURCE_DIR}/OrientationMath/SourceList.cmake)

set(OrientationLib_SRCS

)
set(OrientationLib_HDRS
    ${OrientationLib_SOURCE_DIR}/OrientationLibConstants.h
)

cmp_IDE_SOURCE_PROPERTIES( "" "${OrientationLib_HDRS}" "${OrientationLib_SRCS}" ${PROJECT_INSTALL_HEADERS})

set(OrientationLib_PROJECT_SRCS
    ${OrientationLib_SRCS}
    ${OrientationLib_HDRS}
    ${OrientationLib_Utilities_HDRS}
    ${OrientationLib_Utilities_SRCS}
    ${OrientationLib_OrientationMath_HDRS}
    ${OrientationLib_OrientationMath_SRCS}
    #-- Our Generated Header Files
    ${OrientationLib_Generated_CMP_SRCS}
)


add_library(${PROJECT_NAME} ${LIB_TYPE} ${OrientationLib_PROJECT_SRCS})
target_link_libraries(${PROJECT_NAME} H5Support SIMPLib ${LAPACK_LIB})

LibraryProperties( ${PROJECT_NAME} ${EXE_DEBUG_EXTENSION} )

set_target_properties (${PROJECT_NAME} PROPERTIES
  FOLDER EMsoftPublic/${PROJECT_NAME}
)

include(GenerateExportHeader)

generate_export_header(${PROJECT_NAME}
  EXPORT_MACRO_NAME ${PROJECT_NAME}_EXPORT
  EXPORT_FILE_NAME ${PROJECT_NAME}DLLExport.h
  )



EMsoft_SetupInstallDirs()

if(BUILD_SHARED_LIBS AND NOT APPLE)
  INSTALL(TARGETS ${PROJECT_NAME}
    COMPONENT Applications
    RUNTIME DESTINATION ${install_dir}
    LIBRARY DESTINATION ${lib_install_dir}
    ARCHIVE DESTINATION ${lib_install_dir}
    BUNDLE DESTINATION "."
  )
endif()
