

include("${EMsoft_SOURCE_DIR}/Source/EMsoft_Functions.cmake")

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  set(FORTRAN_LIBRARIES gomp gcc_eh)
endif()


include_directories("${EMsoftHDFLib_BINARY_DIR}")
set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/Utilities")
set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")

if(EMsoft_ENABLE_HDF5_SUPPORT)
  
  string(TOLOWER ${LIB_TYPE} lib_type)
  set(hdf5LinkLibs hdf5 hdf5_f90cstub hdf5_fortran hdf5_hl hdf5_hl_f90cstub hdf5_hl_fortran)
  set(comps ${hdf5LinkLibs})
  if (HDF5_VERSION_STRING VERSION_GREATER 1.8.15)
    set(hdf5LinkLibs "")
    foreach(comp ${comps})
      if(TARGET ${comp}-${lib_type})
        set(hdf5LinkLibs ${hdf5LinkLibs} ${comp}-${lib_type})
      endif()
    endforeach()
  endif()

  set(EXE_LINK_LIBRARIES ${EXE_LINK_LIBRARIES} ${hdf5LinkLibs} EMsoftHDFLib)

      Add_EMsoft_Executable(TARGET EMDisorientations
                                    SOURCES ${APP_DIR}/EMDisorientations.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMDisorientationsTwoPhase
                                    SOURCES ${APP_DIR}/EMDisorientationsTwoPhase.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMConvertOrientations
                                    SOURCES ${APP_DIR}/EMConvertOrientations.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMeqvrot
                                    SOURCES ${APP_DIR}/EMeqvrot.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMqg 
                                    SOURCES ${APP_DIR}/EMqg.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMstar 
                                    SOURCES ${APP_DIR}/EMstar.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMorbit 
                                    SOURCES ${APP_DIR}/EMorbit.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMzap 
                                    SOURCES ${APP_DIR}/EMzap.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMZAgeom 
                                    SOURCES ${APP_DIR}/EMZAgeom.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMorient 
                                    SOURCES ${APP_DIR}/EMorient.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMlatgeom 
                                    SOURCES ${APP_DIR}/EMlatgeom.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMstereo 
                                    SOURCES ${APP_DIR}/EMstereo.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMmkxtal 
                                    SOURCES ${APP_DIR}/EMmkxtal.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMdrawcell 
                                    SOURCES ${APP_DIR}/EMdrawcell.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMKikuchiMap 
                                    SOURCES ${APP_DIR}/EMKikuchiMap.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMfamily 
                                    SOURCES ${APP_DIR}/EMfamily.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMxtalinfo 
                                    SOURCES ${APP_DIR}/EMxtalinfo.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMsoft_Executable(TARGET EMHOLZ 
                                    SOURCES ${APP_DIR}/EMHOLZ.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

      Add_EMSoft_Executable(TARGET EMmergefiles
                                    SOURCES ${APP_DIR}/EMmergefiles.f90 
                                    LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                                    SOLUTION_FOLDER EMsoftPublic/Utilities 
                                    INSTALL_PROGRAM TRUE)

endif()

Add_EMsoft_Executable(TARGET EMlistSG 
                              SOURCES ${APP_DIR}/EMlistSG.f90 
                              LINK_LIBRARIES EMsoftLib
                              SOLUTION_FOLDER EMsoftPublic/Utilities 
                              INSTALL_PROGRAM TRUE)

Add_EMSoft_Executable(TARGET EMOpenCLinfo 
                              SOURCES ${APP_DIR}/EMOpenCLinfo.f90 
                              LINK_LIBRARIES EMsoftLib
                              SOLUTION_FOLDER EMsoftPublic/Utilities 
                              INSTALL_PROGRAM TRUE)
Add_EMsoft_Executable(TARGET EMsoftinit 
                              SOURCES ${APP_DIR}/EMsoftinit.f90 
                              LINK_LIBRARIES EMsoftLib
                              SOLUTION_FOLDER EMsoftPublic/Utilities 
                              INSTALL_PROGRAM TRUE)
Add_EMSoft_Executable(TARGET EMsampleRFZ 
                              SOURCES ${APP_DIR}/EMsampleRFZ.f90 
                              LINK_LIBRARIES EMsoftLib
                              SOLUTION_FOLDER EMsoftPublic/Utilities 
                              INSTALL_PROGRAM TRUE)

