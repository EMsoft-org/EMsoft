set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/QC")
set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})



if(EMsoft_ENABLE_HDF5_SUPPORT)

  

  
  GetHDF5LinkLibraries(EMSOFT)
  set(EXE_LINK_LIBRARIES ${EMSOFT_hdf5LinkLibs} EMsoftHDFLib)

  Add_EMsoft_Executable(TARGET EMECPQCmaster
                        SOURCES ${APP_DIR}/EMECPQCmaster.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}  
                        TEMPLATE ${TMPLT_DIR}/EMECPQCmaster.template 
                        SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/QC
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                      )

  Add_EMsoft_Executable(TARGET EMEBSDQCmaster
                        SOURCES ${APP_DIR}/EMEBSDQCmaster.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}  
                        TEMPLATE ${TMPLT_DIR}/EMEBSDQCmaster.template 
                        SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/QC
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMEBSD2DQCmaster
                        SOURCES ${APP_DIR}/EMEBSD2DQCmaster.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}  
                        TEMPLATE ${TMPLT_DIR}/EMEBSD2DQCmaster.template 
                        SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/QC
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMmkqxtal
                        SOURCES ${APP_DIR}/EMmkqxtal.f90 
                        LINK_LIBRARIES EMsoftHDFLib
                        SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/QC
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMECPQC
                        SOURCES ${APP_DIR}/EMECPQC.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}  
                        TEMPLATE ${TMPLT_DIR}/EMECP.template 
                        SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMCBEDQC
                        SOURCES ${APP_DIR}/EMCBEDQC.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES}  
                        TEMPLATE ${TMPLT_DIR}/EMCBEDQC.template  
                        SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/QC
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )  

  Add_EMsoft_Executable(TARGET EMCBED2DQC
                        SOURCES ${APP_DIR}/EMCBED2DQC.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES}  
                        TEMPLATE ${TMPLT_DIR}/EMCBED2DQC.template  
                        SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/QC
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )   
endif()
