project(EMsoftLib)

include (${CMP_SOURCE_DIR}/cmpCMakeMacros.cmake 
${EMsoftLib_BINARY_DIR}/StringConstants.cmake)

# configure_file(${EMsoftLib_SOURCE_DIR}/StringConstants.cmake
#                ${EMsoftLib_BINARY_DIR}/StringConstants.cmake @ONLY)

cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${EMsoftLib_BINARY_DIR}/StringConstants.cmake
                             CONFIGURED_TEMPLATE_PATH   ${EMsoftLib_SOURCE_DIR}/StringConstants.cmake )


add_custom_target(EMsoftLib_Generate_StringConstants ALL 
                  "${CMAKE_COMMAND}" -P "${EMsoftLib_BINARY_DIR}/StringConstants.cmake" 
                  COMMENT "Generating StringConstants files...."
                  )



if(0)
  # fetch SHT file format content
  # this could be done with a git submodule but the user would have to call:
  #   git submodule update
  #   git clone --recursive
  # which is a bit annoying
  include(FetchContent)
  FetchContent_Declare(
    SHTfile
    GIT_REPOSITORY "https://github.com/EMsoft-org/SHTfile"
    # GIT_TAG        "v3.1.2" # just get the most recent version for now
    # GIT_PROGRESS   TRUE # its currently only 1 file, we probably don't need to print out progress
  )
  # set(FETCHCONTENT_QUIET NO) # again only 1 file
  FetchContent_MakeAvailable(SHTfile)
  FetchContent_GetProperties(SHTfile BINARY_DIR SHTfile_BINARY_DIR)
  include_directories(${SHTfile_BINARY_DIR})
endif()





#------------------------------------------------------------------------------
# Setup all the source files
set(EMsoftLib_SRCS
  ${EMsoftLib_SOURCE_DIR}/bmp.f90
  ${EMsoftLib_SOURCE_DIR}/bobyqa_refinement.f90
  ${EMsoftLib_SOURCE_DIR}/bobyqa.f90
  # ${EMsoftLib_SOURCE_DIR}/CLsupport.f90
  ${EMsoftLib_SOURCE_DIR}/configmod.f90
  ${EMsoftLib_SOURCE_DIR}/constants.f90
  ${EMsoftLib_SOURCE_DIR}/crystal.f90
  ${EMsoftLib_SOURCE_DIR}/defectmodule.f90
  ${EMsoftLib_SOURCE_DIR}/detectors.f90
  ${EMsoftLib_SOURCE_DIR}/dictmod.f90
  ${EMsoftLib_SOURCE_DIR}/diffraction.f90
  ${EMsoftLib_SOURCE_DIR}/diffractionQC.f90
  ${EMsoftLib_SOURCE_DIR}/distortion.f90
  ${EMsoftLib_SOURCE_DIR}/ECCImod.f90
  ${EMsoftLib_SOURCE_DIR}/eispack.f90
  ${EMsoftLib_SOURCE_DIR}/EMdymod.f90
  ${EMsoftLib_SOURCE_DIR}/error.f90
  ${EMsoftLib_SOURCE_DIR}/fftw3mod.f90
  ${EMsoftLib_SOURCE_DIR}/files.f90
  ${EMsoftLib_SOURCE_DIR}/filters.f90
  ${EMsoftLib_SOURCE_DIR}/GBmod.f90
  ${EMsoftLib_SOURCE_DIR}/graphics.f90
  ${EMsoftLib_SOURCE_DIR}/gvectors.f90
  ${EMsoftLib_SOURCE_DIR}/gvectorsQC.f90
  ${EMsoftLib_SOURCE_DIR}/hhmod.f90
  ${EMsoftLib_SOURCE_DIR}/image_io.f90
  ${EMsoftLib_SOURCE_DIR}/image.f90
  ${EMsoftLib_SOURCE_DIR}/imageOPs.f90
  # ${EMsoftLib_SOURCE_DIR}/Indexingmod.f90
  ${EMsoftLib_SOURCE_DIR}/initializers.f90
  ${EMsoftLib_SOURCE_DIR}/io.f90
  ${EMsoftLib_SOURCE_DIR}/JSONsupport.f90
  ${EMsoftLib_SOURCE_DIR}/kvectors.f90
  ${EMsoftLib_SOURCE_DIR}/kvectorsQC.f90
  ${EMsoftLib_SOURCE_DIR}/Lambert.f90
  ${EMsoftLib_SOURCE_DIR}/Laue.f90
  ${EMsoftLib_SOURCE_DIR}/lzw.f90
  ${EMsoftLib_SOURCE_DIR}/math.f90
  ${EMsoftLib_SOURCE_DIR}/MBmodule.f90
  ${EMsoftLib_SOURCE_DIR}/MBmoduleQC.f90
  ${EMsoftLib_SOURCE_DIR}/MRCmod.f90
  ${EMsoftLib_SOURCE_DIR}/MuellerCalculus.f90
  ${EMsoftLib_SOURCE_DIR}/multibeams.f90
  ${EMsoftLib_SOURCE_DIR}/NameListHandlers.f90
  ${EMsoftLib_SOURCE_DIR}/NameListTypedefs.f90
  ${EMsoftLib_SOURCE_DIR}/noise.f90
  ${EMsoftLib_SOURCE_DIR}/notifications.f90
  ${EMsoftLib_SOURCE_DIR}/others.f90
  ${EMsoftLib_SOURCE_DIR}/PEDmod.f90
  ${EMsoftLib_SOURCE_DIR}/PFInversionmod.f90
  ${EMsoftLib_SOURCE_DIR}/pgm.f90
  ${EMsoftLib_SOURCE_DIR}/postscript.f90
  ${EMsoftLib_SOURCE_DIR}/povray.f90
  ${EMsoftLib_SOURCE_DIR}/QCmod.f90
  ${EMsoftLib_SOURCE_DIR}/qcrystal.f90
  ${EMsoftLib_SOURCE_DIR}/quaternions.f90
  ${EMsoftLib_SOURCE_DIR}/random.f90
  ${EMsoftLib_SOURCE_DIR}/rng.f90
  ${EMsoftLib_SOURCE_DIR}/rotations.f90
  ${EMsoftLib_SOURCE_DIR}/simulated_annealing.f90
  ${EMsoftLib_SOURCE_DIR}/so2.f90
  ${EMsoftLib_SOURCE_DIR}/so3.f90
  ${EMsoftLib_SOURCE_DIR}/spsort.f
  ${EMsoftLib_SOURCE_DIR}/STEMmodule.f90
  ${EMsoftLib_SOURCE_DIR}/symmetry.f90
  ${EMsoftLib_SOURCE_DIR}/tif.f90
  ${EMsoftLib_SOURCE_DIR}/tiff.f90
  ${EMsoftLib_SOURCE_DIR}/timing.f90
  ${EMsoftLib_SOURCE_DIR}/typedefs.f90
  ${EMsoftLib_SOURCE_DIR}/xrd.f90
  ${EMsoftLib_SOURCE_DIR}/random.f90
  ${EMsoftLib_SOURCE_DIR}/imageOPs.f90
  ${EMsoftLib_SOURCE_DIR}/hhmod.f90
  ${EMsoftLib_SOURCE_DIR}/detectors.f90
  ${EMsoftLib_SOURCE_DIR}/DSHT.f90
  ${EMsoftLib_SOURCE_DIR}/fft_wrap.f90
  ${EMsoftLib_SOURCE_DIR}/ISEmod.f90

  ${EMsoftLib_Additional_SRCS}
)

set(EMsoftLib_C_SRCS
  ${EMsoftLib_SOURCE_DIR}/mbir.c
  ${EMsoftLib_SOURCE_DIR}/mbirHeader.h
  ${EMsoftLib_SOURCE_DIR}/denoise.c
  ${EMsoftLib_SOURCE_DIR}/bclsf90wrapper.c
  ${EMsoftLib_SOURCE_DIR}/cs_malloc.c
  ${EMsoftLib_SOURCE_DIR}/cs_util.c
  ${EMsoftLib_SOURCE_DIR}/cs.h
  ${EMsoftLib_Additional_C_SRCS}
)

set(EMsoftLib_Cpp_SRCS
  ${EMsoftLib_SOURCE_DIR}/sht_file.cpp
  ${SHTfile_BINARY_DIR}/sht_file.hpp
)

if(NOT WIN32 AND BUILD_SHARED_LIBS)
  set_source_files_properties(${EMsoftLib_C_SRCS} PROPERTIES COMPILE_FLAGS -fPIC)
  set_source_files_properties(${EMsoftLib_Cpp_SRCS} PROPERTIES COMPILE_FLAGS -fPIC)
endif()

if(EMsoft_ENABLE_TESTING)
  set(EMsoft_TESTING_DIR "${EMsoft_BINARY_DIR}/Testing")
  file(MAKE_DIRECTORY ${EMsoft_TESTING_DIR})
endif()

message(STATUS "HDF5_VERSION_MAJOR: ${HDF5_VERSION_MAJOR}")

if(${HDF5_VERSION_MAJOR} EQUAL 1.10)
set(EMsoft_HDF5_110_DEFINES "
! For HDF5 1.10, these are needed
! Define the \"kind\" parameters for Fortran_REAL_4
  integer,parameter                     :: Fortran_REAL_4 = 4

! Define the \"kind\" parameters for Fortran_REAL_4
  integer,parameter                     :: Fortran_REAL_8 = 8
")
endif()
         
cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${EMsoftLib_BINARY_DIR}/local.f90
                              CONFIGURED_TEMPLATE_PATH   ${EMsoftLib_SOURCE_DIR}/local.f90.in  )
#-------------------------------------------------------------------------------
# Add files GENERATED DURING CMAKE TIME
set(EMsoftLib_SRCS
  ${EMsoftLib_SRCS}
  ${EMsoftLib_BINARY_DIR}/local.f90  
  ${EMsoftLib_BINARY_DIR}/stringconstants.f90 
)

#-------------------------------------------------------------------------------
# Put just the *.c files into their own library so Visual Studio will compile
# everything correctly. VS can NOT have both fortran and C files in the same target
add_library(EMsoftLib_C STATIC ${EMsoftLib_C_SRCS})
set_target_properties (EMsoftLib_C PROPERTIES
  LINKER_LANGUAGE Fortran
   # Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  FOLDER EMsoftPublic
)
add_library(EMsoftLib_Cpp STATIC ${EMsoftLib_Cpp_SRCS})
set_target_properties (EMsoftLib_Cpp PROPERTIES
  LINKER_LANGUAGE Fortran
   # Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  FOLDER EMsoftPublic
)
target_include_directories(EMsoftLib_Cpp PRIVATE ${SHTfile_BINARY_DIR})

add_library(EMsoftLib ${LIB_TYPE} ${EMsoftLib_SRCS})
target_link_libraries(EMsoftLib
                EMsoftLib_C 
                EMsoftLib_Cpp 
                jsonfortran 
                bcls::bcls            
)
if (Fortran_COMPILER_NAME MATCHES "ifort.*")
  target_link_libraries(EMsoftLib
                  ${MKL_LIBRARIES}
  )
endif()

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  target_link_libraries(EMsoftLib
                  ${FFTW3_LIBRARY} 
                  ${EMsoft_BLAS_LAPACK_LIBS}
                  ${EMsoft_FORTRAN_SUPPORT_LIBS}
  )
endif()


target_include_directories(EMsoftLib PUBLIC ${CLFortran_INCLUDE_DIRS} ${FFTW3_INCLUDE_DIR})

set_target_properties (EMsoftLib PROPERTIES
                  LINKER_LANGUAGE Fortran
                   # Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
                  FOLDER EMsoftPublic
                  )
set_source_files_properties(${EMsoftLib_BINARY_DIR}/stringconstants.f90 PROPERTIES GENERATED TRUE)
add_dependencies(EMsoftLib EMsoftLib_Generate_StringConstants)

# On Windows we need a few extra compile and linker flags. By making these PUBLIC they should
# flow down to every dependent target 
if(WIN32)
  target_compile_options(EMsoftLib PUBLIC 
                          $<$<COMPILE_LANGUAGE:Fortran>:/assume:byterecl>
                          $<$<COMPILE_LANGUAGE:Fortran>:/Qopenmp>
                          $<$<COMPILE_LANGUAGE:Fortran>:/Qdiag-disable:11082>
                          $<$<COMPILE_LANGUAGE:Fortran>:/Qdiag-disable:7025>
                          $<$<COMPILE_LANGUAGE:Fortran>:/Qip>
                        #  $<$<COMPILE_LANGUAGE:Fortran>:/heap-arrays>
                          )
  target_link_options(EMsoftLib PUBLIC /STACK:2000000000 /machine:x64)
else()
  if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
    # First, Flags that should be used under any build condition
    target_compile_options(EMsoftLib PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-fopenmp>
        $<$<COMPILE_LANGUAGE:Fortran>:-fno-sign-zero>
        $<$<COMPILE_LANGUAGE:Fortran>:-fno-f2c>
        $<$<COMPILE_LANGUAGE:Fortran>:-std=gnu>
    )
    # Next, DEBUG compiler options.
    target_compile_options(EMsoftLib PUBLIC $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-g>
                          $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-fcheck=bounds>
                          $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-O0>
    )
    # Next, RELEASE compiler options.
    target_compile_options(EMsoftLib PUBLIC $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-funroll-all-loops>
                          $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3>
    )

  elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # First, Flags that should be used under any build condition
    target_compile_options(EMsoftLib PUBLIC 
                                            #$<$<COMPILE_LANGUAGE:Fortran>:-heap-arrays>
                                            $<$<COMPILE_LANGUAGE:Fortran>:-qopenmp>
                                            $<$<COMPILE_LANGUAGE:Fortran>:-assume byterecl>
                                            $<$<COMPILE_LANGUAGE:Fortran>:-stand f08 -diag-disable=7025>
    )
    # Next, DEBUG compiler options.
    target_compile_options(EMsoftLib PUBLIC $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-debug all> 
                                            $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-O0> 
                                            $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-g>
    )
    # Next, RELEASE compiler options. 
    target_compile_options(EMsoftLib PUBLIC $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3>)
  else()
    message(WARNING "Fortran compiler not recognized. ${Fortran_COMPILER_NAME}")
  endif()
endif()


#-------------------------------------------------------------------------------
# This sets up the two variables install_dir and lib_install_dir
EMsoft_SetupInstallDirs()

#if(BUILD_SHARED_LIBS)
  INSTALL(TARGETS EMsoftLib EMsoftLib_C EMsoftLib_Cpp
    COMPONENT Applications
    EXPORT EMsoftLibTargets
    RUNTIME DESTINATION ${install_dir}
    LIBRARY DESTINATION ${lib_install_dir}
    ARCHIVE DESTINATION ${lib_install_dir}
    BUNDLE DESTINATION "."
  )
#endif()


install(FILES ${EMsoftLib_BINARY_DIR}/EMsoftStringConstants.h
      DESTINATION ${include_install_dir}/EMsoftLib
      COMPONENT Applications
)

if(APPLE AND BUILD_SHARED_LIBS)
  set(QAB_INSTALL_DEST "lib")
  set(osx_app_name "libEMsoftLib.dylib")
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
include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftLibTargetsConfigVersion.cmake"
  VERSION ${EMsoft_VERSION}
  COMPATIBILITY AnyNewerVersion
)
#if(BUILD_SHARED_LIBS)
  export(EXPORT EMsoftLibTargets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftLibTargets.cmake"
    NAMESPACE EMsoft::
  )
#endif()

configure_file(cmake/EMsoftConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftConfig.cmake"
  @ONLY
)

set(ConfigPackageLocation ${lib_install_dir}/cmake/EMsoft)

#if(BUILD_SHARED_LIBS)
  install(EXPORT EMsoftLibTargets
    FILE
      EMsoftLibTargets.cmake
    NAMESPACE
      EMsoft::
    DESTINATION
      ${ConfigPackageLocation}
  )

#endif()

install(
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/EMsoft/EMsoftLibTargetsConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
  COMPONENT
    Devel
)




