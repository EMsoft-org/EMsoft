set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/GBs")
set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})


if(EMsoft_ENABLE_HDF5_SUPPORT)

  GetHDF5LinkLibraries(EMSOFT)
  set(EXE_LINK_LIBRARIES  ${EMSOFT_hdf5LinkLibs} EMsoftHDFLib)

  Add_EMsoft_Executable(TARGET EMGBO 
                        SOURCES ${APP_DIR}/EMGBO.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMGBO.template 
                        SOLUTION_FOLDER EMsoftPublic/GBs
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMGBOdm 
                        SOURCES ${APP_DIR}/EMGBOdm.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMGBOdm.template 
                        SOLUTION_FOLDER EMsoftPublic/GBs 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )


  Add_EMsoft_Executable(TARGET EMoSLERP
                        SOURCES ${APP_DIR}/EMoSLERP.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                        TEMPLATE ${TMPLT_DIR}/EMoSLERP.template 
                        SOLUTION_FOLDER EMsoftPublic/GBs
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )
endif()
