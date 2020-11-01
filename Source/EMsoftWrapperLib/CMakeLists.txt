
project(EMsoftWrapperLib C CXX Fortran)

# --------------------------------------------------------------------
# The MODALITY_DIRS variable is set one level above this one.

# --------------------------------------------------------------------
# Add the executables
foreach(MODALITY ${MODALITY_DIRS})
  if( "${EMsoft_ENABLE_${MODALITY}}" STREQUAL "ON" )
    # message(STATUS "${MODALITY} Wrapper Lib: ${EMsoft_ENABLE_${MODALITY}}")
    include( ${PROJECT_SOURCE_DIR}/${MODALITY}/SourceList.cmake)
  endif()
endforeach()


set(EMsoftWrapperLib_SOURCES
  ${DictionaryIndexing_Wrapper_SRCS}
  ${EEC_Wrapper_SRCS}
  ${OLIO_Wrapper_SRCS}
  ${OM_Wrapper_SRCS}
  ${SEM_Wrapper_SRCS}
  ${TEM_Wrapper_SRCS}
  ${QC_Wrapper_SRCS}
  ${Utilities_Wrapper_SRCS}
)

set(EMsoftWrapperLib_HEADERS
  ${DictionaryIndexing_Wrapper_HEADERS} 
  ${SEM_Wrapper_HEADERS} 
  ${TEM_Wrapper_HEADERS} 
)


add_library(EMsoftWrapperLib ${LIB_TYPE} ${EMsoftWrapperLib_SOURCES} ${EMsoftWrapperLib_HEADERS})
target_link_libraries(EMsoftWrapperLib EMsoftHDFLib EMOpenCLLib)

target_include_directories(EMsoftWrapperLib 
      PUBLIC ${FFTW3_INCLUDE_DIR} 
      PUBLIC ${CLFORTRAN_INCLUDE_DIR}
      ${CLFortran_INSTALL}/include
      $<BUILD_INTERFACE:${EMsoftHDFLib_BINARY_DIR}>
      $<BUILD_INTERFACE:${EMOpenCLLib_BINARY_DIR}>
)

set_target_properties (EMsoftWrapperLib PROPERTIES
  LINKER_LANGUAGE Fortran
   # Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  FOLDER EMsoftWrapperPublic
)


#---------------------------------------------------------------------
# This sets up the two variables install_dir and lib_install_dir
EMsoft_SetupInstallDirs()

#if(BUILD_SHARED_LIBS)
  INSTALL(TARGETS EMsoftWrapperLib
    COMPONENT Applications
    EXPORT EMsoftWrapperLibTargets
    RUNTIME DESTINATION ${install_dir}
    LIBRARY DESTINATION ${lib_install_dir}
    ARCHIVE DESTINATION ${lib_install_dir}
    BUNDLE DESTINATION "."
  )
#endif()


install(FILES ${EMsoftWrapperLib_HEADERS}
      DESTINATION ${include_install_dir}/EMsoftWrapperLib
      COMPONENT Applications
)

if(APPLE AND BUILD_SHARED_LIBS)
  set(QAB_INSTALL_DEST "lib")
  set(osx_app_name "libEMsoftWrapperLib.dylib")
  get_property(EMsoftBinaryDir GLOBAL PROPERTY EMsoftBinaryDir)
  set(OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT "${EMsoft_BINARY_DIR}/EMsoftWrapperLib/EMsoftWrapperLib_CompleteTool.cmake")
  set(OPTIMIZE_BUNDLE_SHELL_SCRIPT            "${EMsoft_BINARY_DIR}/EMsoftWrapperLib/EMsoftWrapperLib_OptimizeTool.sh")
  set(PROJECT_INSTALL_DIR "lib")
  configure_file("${CMP_OSX_TOOLS_SOURCE_DIR}/CompleteTool.cmake.in"
                "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" @ONLY IMMEDIATE)
  configure_file("${CMP_OSX_TOOLS_SOURCE_DIR}/CompleteTool.sh.in"
                "${OPTIMIZE_BUNDLE_SHELL_SCRIPT}" @ONLY IMMEDIATE)
  #install(SCRIPT "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" COMPONENT ${QAB_COMPONENT})
endif()


# --------------------------------------------------------------------
include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftWrapperLibTargetsConfigVersion.cmake"
  VERSION ${EMsoft_VERSION}
  COMPATIBILITY AnyNewerVersion
)
#if(BUILD_SHARED_LIBS)
  export(EXPORT EMsoftWrapperLibTargets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftWrapperLibTargets.cmake"
    NAMESPACE EMsoft::
  )
#endif()

configure_file(cmake/EMsoftWrapperConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftWrapperConfig.cmake"
  @ONLY
)

set(ConfigPackageLocation ${lib_install_dir}/cmake/EMsoft)

#if(BUILD_SHARED_LIBS)
  install(EXPORT EMsoftWrapperLibTargets
    FILE
      EMsoftWrapperLibTargets.cmake
    NAMESPACE
      EMsoft::
    DESTINATION
      ${ConfigPackageLocation}
  )

#endif()

install(
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftWrapperLibTargetsConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
  COMPONENT
    Devel
)

