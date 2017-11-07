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

include_directories("${EMsoftLib_BINARY_DIR}")
include_directories("${EMsoft_SOURCE_DIR}/Source")

include_directories(${HDF5_INCLUDE_DIRS})
if (HDF5_VERSION_STRING VERSION_GREATER 1.8.15)
  if(${HDF5_BUILD_FORTRAN})
    if(${HDF5_BUILD_SHARED_LIBS})
      include_directories("${HDF5_INSTALL}/include/shared")
    else()
      include_directories("${HDF5_INSTALL}/include/static")
    endif()
  endif()
endif()

message(STATUS "HDF5_INSTALL: ${HDF5_INSTALL}")
set(EMsoftHDFLib_SRCS
	${EMsoftHDFLib_SOURCE_DIR}/initializersHDF.f90
 	${EMsoftHDFLib_SOURCE_DIR}/NameListHDFwriters.f90
	${EMsoftHDFLib_SOURCE_DIR}/EBSDmod.f90
 	${EMsoftHDFLib_SOURCE_DIR}/EMdymodHDF.f90
  ${EMsoftHDFLib_SOURCE_DIR}/ECPmod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/EBSDiomod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/ECPiomod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/ECPmod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/EBSDiomod.f90
 	${EMsoftHDFLib_SOURCE_DIR}/HDFsupport.f90
 	${EMsoftHDFLib_SOURCE_DIR}/EBSDDImod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/EMh5ebsd.f90
  ${EMsoftHDFLib_SOURCE_DIR}/FitOrientations.f90
  ${EMsoftHDFLib_SOURCE_DIR}/TKDmod.f90
  ${EMsoftHDFLib_SOURCE_DIR}/TKDDImod.f90

  ${EMsoftHDFLib_Additional_SRCS}
)

set(lib_type "static")
if(${${HDF5_PACKAGE_NAME}_BUILD_SHARED_LIBS})
  set(lib_type "shared")
endif()

GetHDF5LinkLibraries(EMSOFT)
add_library(EMsoftHDFLib ${LIB_TYPE} ${EMsoftHDFLib_SRCS})
target_link_libraries(EMsoftHDFLib ${EMSOFT_hdf5LinkLibs} EMsoftLib)

set_target_properties (EMsoftHDFLib PROPERTIES
  LINKER_LANGUAGE Fortran
   # Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  FOLDER EMsoftPublic
)

if(APPLE AND BUILD_SHARED_LIBS)
  # set_target_properties(EMsoftLib PROPERTIES MACOSX_RPATH TRUE)
  # set_target_properties(EMsoftLib PROPERTIES INSTALL_NAME_DIR "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
  # set_target_properties(EMsoftLib PROPERTIES BUILD_WITH_INSTALL_RPATH ON)
endif()

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

