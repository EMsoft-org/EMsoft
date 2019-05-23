
set(EXE_LINK_LIBRARIES "EMsoftLib")

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  set(EXE_LINK_LIBRARIES ${EXE_LINK_LIBRARIES} gomp gcc_eh)
endif()

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
  if(WIN32)
    add_definitions("/Qopenmp /Qdiag-disable:11082 /Qip")
  endif()
endif()

include_directories("${EMsoftHDFLib_BINARY_DIR}")

set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/DictionaryIndexing")
set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})


if(EMsoft_ENABLE_HDF5_SUPPORT)
  
  GetHDF5LinkLibraries(EMSOFT)
  set(EXE_LINK_LIBRARIES ${EXE_LINK_LIBRARIES} ${EMSOFT_hdf5LinkLibs} EMsoftHDFLib EMsoftLib)


  Add_EMsoft_Executable(TARGET EMDPFit
                      SOURCES ${APP_DIR}/EMDPFit.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                      TEMPLATE ${TMPLT_DIR}/EMDPFit.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)

  Add_EMsoft_Executable(TARGET EMECPDI
                      SOURCES ${APP_DIR}/EMECPDI.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                      TEMPLATE ${TMPLT_DIR}/EMECPDI.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)

  Add_EMsoft_Executable(TARGET EMEBSDDI
                      SOURCES ${APP_DIR}/EMEBSDDI.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                      TEMPLATE ${TMPLT_DIR}/EMEBSDDI.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)

  Add_EMsoft_Executable(TARGET EMTKDDI
                      SOURCES ${APP_DIR}/EMTKDDI.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                      TEMPLATE ${TMPLT_DIR}/EMTKDDI.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)
 
  Add_EMsoft_Executable(TARGET EMFitOrientation
                      SOURCES ${APP_DIR}/EMFitOrientation.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                      TEMPLATE ${TMPLT_DIR}/EMFitOrientation.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)
 
  Add_EMsoft_Executable(TARGET EMgetCTF
                      SOURCES ${APP_DIR}/EMgetCTF.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                      TEMPLATE ${TMPLT_DIR}/EMgetCTF.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)

  Add_EMsoft_Executable(TARGET EMgetANG
                      SOURCES ${APP_DIR}/EMgetANG.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                      TEMPLATE ${TMPLT_DIR}/EMgetANG.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)

  Add_EMsoft_Executable(TARGET EMgetEulers
                      SOURCES ${APP_DIR}/EMgetEulers.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                      TEMPLATE ${TMPLT_DIR}/EMgetEulers.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)
 
  Add_EMsoft_Executable(TARGET EMEBSDDIpreview
                      SOURCES ${APP_DIR}/EMEBSDDIpreview.f90 
                      LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}  
                      TEMPLATE ${TMPLT_DIR}/EMEBSDDIpreview.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)

  Add_EMsoft_Executable(TARGET EMEBSDDIchangesetting
                      SOURCES ${APP_DIR}/EMEBSDDIchangesetting.f90 
                      LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}  
                      TEMPLATE ${TMPLT_DIR}/EMEBSDDIchangesetting.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)

  Add_EMsoft_Executable(TARGET EMgetADP
                        SOURCES ${APP_DIR}/EMgetADP.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES} ${FORTRAN_LIBRARIES}
                        TEMPLATE ${TMPLT_DIR}/EMgetADP.template 
                        SOLUTION_FOLDER EMsoftPublic/DictionaryIndexing 
                        INSTALL_PROGRAM TRUE)

  Add_EMsoft_Executable(TARGET EMdpextract
                        SOURCES ${APP_DIR}/EMdpextract.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES} ${FORTRAN_LIBRARIES}
                        SOLUTION_FOLDER EMsoftPublic/DictionaryIndexing 
                        INSTALL_PROGRAM TRUE)

endif()
