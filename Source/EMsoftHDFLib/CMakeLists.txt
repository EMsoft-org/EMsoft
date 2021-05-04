include(${PROJECT_SOURCE_DIR}/Source/EMsoft_Functions.cmake)

project(EMsoftHDFLib)



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



set(EMsoftHDFLib_SRCS
	${EMsoftHDFLib_SOURCE_DIR}/EBSDmod.f90
	${EMsoftHDFLib_SOURCE_DIR}/initializersHDF.f90
 	${EMsoftHDFLib_SOURCE_DIR}/EBSDDImod.f90
 	${EMsoftHDFLib_SOURCE_DIR}/HDFsupport.f90
 	${EMsoftHDFLib_SOURCE_DIR}/NameListHDFwriters.f90
  ${EMsoftHDFLib_Additional_SRCS}
  ${EMsoftHDFLib_SOURCE_DIR}/commonmod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/EBSDiomod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/ECPiomod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/ECPmod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/ECPmod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/EMh5ebsd.f90
  ${EMsoftHDFLib_SOURCE_DIR}/FitOrientations.f90
  ${EMsoftHDFLib_SOURCE_DIR}/InitializersQCHDF.f90
  ${EMsoftHDFLib_SOURCE_DIR}/Lauemod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/patternmod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/PFInversionHDF.f90
  ${EMsoftHDFLib_SOURCE_DIR}/QCmodHDF.f90
  ${EMsoftHDFLib_SOURCE_DIR}/TKDDImod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/TKDmod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/utilities.f90
  ${EMsoftHDFLib_SOURCE_DIR}/hhHDFmod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/MDsubroutines.f90

# 	${EMsoftHDFLib_SOURCE_DIR}/EMdymodHDF.f90
)

GetHDF5LinkLibraries(EMSOFT)

add_library(EMsoftHDFLib ${LIB_TYPE} ${EMsoftHDFLib_SRCS})

target_include_directories(EMsoftHDFLib 
                          PUBLIC 
                            ${HDF5_INCLUDE_DIR}
                            ${HDF5_INCLUDE_DIR_FORTRAN}
                            $<BUILD_INTERFACE:${EMsoftLib_BINARY_DIR}>
                          PRIVATE
                            "${EMsoft_SOURCE_DIR}/Source"                            
                    )

target_link_libraries(EMsoftHDFLib 
                          ${EMSOFT_hdf5LinkLibs} 
                          EMsoftLib 
)

set_target_properties (EMsoftHDFLib PROPERTIES
  LINKER_LANGUAGE Fortran
  FOLDER ${EMSOFTPUBLIC_DIR_NAME}
  BUILD_RPATH "${EMsoft_OpenMP_LIB_DIR}"
)

EMsoft_SetupInstallDirs()

#if(BUILD_SHARED_LIBS)
    INSTALL(TARGETS EMsoftHDFLib
      EXPORT EMsoftHDFLibTargets
      COMPONENT Applications
      RUNTIME DESTINATION ${install_dir}
      LIBRARY DESTINATION ${lib_install_dir}
      ARCHIVE DESTINATION ${lib_install_dir}
      BUNDLE DESTINATION "."
  )
#endif()

if(APPLE AND BUILD_SHARED_LIBS)
  set(QAB_INSTALL_DEST "lib")
  set(osx_app_name "libEMsoftHDFLib.dylib")
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
  "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftLibTargetsConfigVersion.cmake"
  VERSION ${EMsoft_VERSION}
  COMPATIBILITY AnyNewerVersion
)
#if(BUILD_SHARED_LIBS)
  export(EXPORT EMsoftHDFLibTargets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftHDFLibTargets.cmake"
    NAMESPACE EMsoft::
  )
#endif()

set(ConfigPackageLocation ${lib_install_dir}/cmake/EMsoft)

#if(BUILD_SHARED_LIBS)
  install(EXPORT EMsoftHDFLibTargets
    FILE
      EMsoftHDFLibTargets.cmake
    NAMESPACE
      EMsoft::
    DESTINATION
      ${ConfigPackageLocation}
  )
#endif()

