include(${PROJECT_SOURCE_DIR}/Source/EMsoft_Functions.cmake)

project(EMOpenCLLib)

set(install_dir "bin")
set(lib_install_dir "lib")

if(APPLE)
  get_property(EMsoft_PACKAGE_DEST_PREFIX GLOBAL PROPERTY EMsoft_PACKAGE_DEST_PREFIX)
  set(install_dir "${EMsoft_PACKAGE_DEST_PREFIX}/bin")
  set(lib_install_dir "${EMsoft_PACKAGE_DEST_PREFIX}/lib")
elseif(WIN32)
  set(install_dir ".")
  set(lib_install_dir ".")
endif()


set(EMOpenCLLib_SRCS
	${EMOpenCLLib_SOURCE_DIR}/CLsupport.f90
	${EMOpenCLLib_SOURCE_DIR}/Indexingmod.f90

# 	${EMOpenCLLib_SOURCE_DIR}/EMdymodHDF.f90
)

add_library(EMOpenCLLib ${LIB_TYPE} ${EMOpenCLLib_SRCS})

target_include_directories(EMOpenCLLib 
                          PUBLIC 
                            ${CLFortran_INSTALL}/include
                            $<BUILD_INTERFACE:${EMsoftLib_BINARY_DIR}>
                            $<BUILD_INTERFACE:${EMsoftHDFLib_BINARY_DIR}>
                          PRIVATE
                            "${EMsoft_SOURCE_DIR}/Source"                            
                    )

if(WIN32)
  target_link_libraries(EMOpenCLLib 
                            ${CLFortran_INSTALL}/lib/clfortran.lib 
                            EMsoftLib 
                            EMsoftHDFLib 
  )
else()
  target_link_libraries(EMOpenCLLib 
                            ${CLFortran_INSTALL}/lib/libclfortran.a 
                            EMsoftLib 
                            EMsoftHDFLib 
  )
endif()

set_target_properties (EMOpenCLLib PROPERTIES
  LINKER_LANGUAGE Fortran
  FOLDER EMsoftPublic
  BUILD_RPATH "${EMsoft_OpenMP_LIB_DIR}"
)

EMsoft_SetupInstallDirs()

#if(BUILD_SHARED_LIBS)
    INSTALL(TARGETS EMOpenCLLib
      EXPORT EMOpenCLLibTargets
      COMPONENT Applications
      RUNTIME DESTINATION ${install_dir}
      LIBRARY DESTINATION ${lib_install_dir}
      ARCHIVE DESTINATION ${lib_install_dir}
      BUNDLE DESTINATION "."
  )
#endif()

if(APPLE AND BUILD_SHARED_LIBS)
  set(QAB_INSTALL_DEST "lib")
  set(osx_app_name "libEMOpenCLLib.dylib")
  get_property(EMsoftBinaryDir GLOBAL PROPERTY EMsoftBinaryDir)
  set(OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT "${EMsoft_BINARY_DIR}/EMsoftLib/EMsoftLib_CompleteTool.cmake")
  set(OPTIMIZE_BUNDLE_SHELL_SCRIPT            "${EMsoft_BINARY_DIR}/EMsoftLib/EMsoftLib_OptimizeTool.sh")
  set(PROJECT_INSTALL_DIR "lib")
  configure_file("${CMP_OSX_TOOLS_SOURCE_DIR}/CompleteTool.cmake.in"
                "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" @ONLY IMMEDIATE)
  configure_file("${CMP_OSX_TOOLS_SOURCE_DIR}/CompleteTool.sh.in"
                "${OPTIMIZE_BUNDLE_SHELL_SCRIPT}" @ONLY IMMEDIATE)
  #install(SCRIPT "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" COMPONENT ${QAB_COMPONENT})
endif()

# --------------------------------------------------------------------
#
include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMOpenCLLibTargetsConfigVersion.cmake"
  VERSION ${EMsoft_VERSION}
  COMPATIBILITY AnyNewerVersion
)
#if(BUILD_SHARED_LIBS)
  export(EXPORT EMOpenCLLibTargets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMOpenCLLibTargets.cmake"
    NAMESPACE EMsoft::
  )
#endif()

set(ConfigPackageLocation ${lib_install_dir}/cmake/EMsoft)

#if(BUILD_SHARED_LIBS)
  install(EXPORT EMOpenCLLibTargets
    FILE
      EMOpenCLLibTargets.cmake
    NAMESPACE
      EMsoft::
    DESTINATION
      ${ConfigPackageLocation}
  )
#endif()

