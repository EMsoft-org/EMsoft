
set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/OM")
set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

if(EMsoft_ENABLE_HDF5_SUPPORT)

  GetHDF5LinkLibraries(EMSOFT)
  set(EXE_LINK_LIBRARIES ${EMSOFT_hdf5LinkLibs} EMsoftHDFLib )

  Add_EMsoft_Executable(TARGET EMCPLMmaster
                              SOURCES ${APP_DIR}/EMCPLMmaster.f90 
                              LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}  
                              TEMPLATE ${TMPLT_DIR}/EMCPLMmaster.template 
                              SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/OM
                              INSTALL_PROGRAM TRUE
                              INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                              )

  Add_EMsoft_Executable(TARGET EMCPLM
                              SOURCES ${APP_DIR}/EMCPLM.f90 
                              LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}  
                              TEMPLATE ${TMPLT_DIR}/EMCPLM.template 
                              SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/OM
                              INSTALL_PROGRAM TRUE
                              INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                              )


endif()
