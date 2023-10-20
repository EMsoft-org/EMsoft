set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/OLIO")
set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})


if((EMsoft_ENABLE_HDF5_SUPPORT) AND (EMsoft_ENABLE_OpenCL_SUPPORT))

  # The libraries are specifically laid out this way in order to ensure the link line
  # has the correct ordering. This seems to be more important on Windows than on macOS/Linux
  set(EXE_LINK_LIBRARIES  
          ${OpenCL_LIBRARY}
          clfortran
          EMsoftLib 
          EMsoftHDFLib
          EMOpenCLLib
  )

  Add_EMsoft_Executable(TARGET EMgammaOpenCL
                        SOURCES ${APP_DIR}/EMgammaOpenCL.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMgamma.template 
                        SOLUTION_FOLDER EMsoftPrivate/OLIO
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR} ${EMOpenCLLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMgammaSTEM
                        SOURCES ${APP_DIR}/EMgammaSTEM.f90 
                        LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}
                        TEMPLATE ${TMPLT_DIR}/EMgammaSTEM.template 
                        SOLUTION_FOLDER EMsoftPrivate/OLIO
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR} ${EMOpenCLLib_BINARY_DIR}
                        )

  # Add_EMsoft_Executable(TARGET EMGBO 
  #                       SOURCES ${APP_DIR}/EMGBO.f90 
  #                       LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES} 
  #                       TEMPLATE ${TMPLT_DIR}/EMGBO.template 
  #                       SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/OLIO 
                          # INSTALL_PROGRAM TRUE
                          # INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                          # )

  # Add_EMsoft_Executable(TARGET EMGBOdm 
  #                       SOURCES ${APP_DIR}/EMGBOdm.f90 
  #                       LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES} 
  #                       TEMPLATE ${TMPLT_DIR}/EMGBOdm.template 
  #                       SOLUTION_FOLDER ${EMSOFTPUBLIC_DIR_NAME}/OLIO 
                          # INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                          # )

elseif(EMsoft_ENABLE_HDF5_SUPPORT)
  
  GetHDF5LinkLibraries(EMSOFT)
  set(EXE_LINK_LIBRARIES ${EMSOFT_hdf5LinkLibs} EMsoftHDFLib)

     Add_EMsoft_Executable(TARGET EMgamma
                           SOURCES ${APP_DIR}/EMgamma.f90 
                           LINK_LIBRARIES jsonfortran ${EXE_LINK_LIBRARIES}  
                           TEMPLATE ${TMPLT_DIR}/EMgamma.template 
                           SOLUTION_FOLDER EMsoftPrivate/OLIO
                           INSTALL_PROGRAM TRUE
                           INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                           )
endif()

