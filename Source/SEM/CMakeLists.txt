set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/SEM")
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

  Add_EMSoft_Executable(TARGET EMMCOpenCL 
                        SOURCES ${APP_DIR}/EMMCOpenCL.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMMCOpenCL.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR} ${EMOpenCLLib_BINARY_DIR}
                        )

  Add_EMSoft_Executable(TARGET EMMCfoil
                        SOURCES ${APP_DIR}/EMMCfoil.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMMfoil.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR} ${EMOpenCLLib_BINARY_DIR}
                        )
 
  Add_EMsoft_Executable(TARGET EMEBSDFull
                        SOURCES ${APP_DIR}/EMEBSDFull.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMEBSDFull.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR} ${EMOpenCLLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMEBSDreflectors 
                        SOURCES ${APP_DIR}/EMEBSDreflectors.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMEBSDreflectors.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR} ${EMOpenCLLib_BINARY_DIR}
                        )
endif()

if(EMsoft_ENABLE_HDF5_SUPPORT)

  GetHDF5LinkLibraries(EMSOFT)
  set(EXE_LINK_LIBRARIES ${EMSOFT_hdf5LinkLibs} EMsoftHDFLib )


  Add_EMsoft_Executable(TARGET EMEBSD 
                        SOURCES ${APP_DIR}/EMEBSD.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMEBSD.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMEBSDmaster 
                        SOURCES ${APP_DIR}/EMEBSDmaster.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                        TEMPLATE ${TMPLT_DIR}/EMEBSDmaster.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMISEmaster 
                        SOURCES ${APP_DIR}/EMISEmaster.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                        TEMPLATE ${TMPLT_DIR}/EMISEmaster.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMEBSDmasterSHT 
                        SOURCES ${APP_DIR}/EMEBSDmasterSHT.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMEBSDmasterSHT.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  
  Add_EMsoft_Executable(TARGET EMECP 
                        SOURCES ${APP_DIR}/EMECP.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMECP.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMECPmaster 
                        SOURCES ${APP_DIR}/EMECPmaster.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMECPmaster.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )
  
  Add_EMsoft_Executable(TARGET EMKosselmaster 
                        SOURCES ${APP_DIR}/EMKosselmaster.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMKosselmaster.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )
  
  Add_EMsoft_Executable(TARGET EMMC 
                        SOURCES ${APP_DIR}/EMMC.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMMC.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )
  
  Add_EMsoft_Executable(TARGET EMECCI 
                        SOURCES ${APP_DIR}/EMECCI.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMECCI.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )
  

  Add_EMsoft_Executable(TARGET EMECPSingle 
                        SOURCES ${APP_DIR}/EMECPSingle.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMECPSingle.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMKAM
                        SOURCES ${APP_DIR}/EMKAM.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMKAM.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMAverageOrient
                        SOURCES ${APP_DIR}/EMAverageOrient.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMAverageOrient.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMOrientationSimilarity
                        SOURCES ${APP_DIR}/EMOrientationSimilarity.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMOrientationSimilarity.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMOrientationViz
                        SOURCES ${APP_DIR}/EMOrientationViz.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMOrientationViz.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMTKD 
                        SOURCES ${APP_DIR}/EMTKD.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMTKD.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )
  
  Add_EMsoft_Executable(TARGET EMTKDmaster 
                        SOURCES ${APP_DIR}/EMTKDmaster.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES} 
                        TEMPLATE ${TMPLT_DIR}/EMTKDmaster.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMEBSDoverlap
                        SOURCES ${APP_DIR}/EMEBSDoverlap.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES}  
                        TEMPLATE ${TMPLT_DIR}/EMEBSDoverlap.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

  Add_EMsoft_Executable(TARGET EMkinematical
                        SOURCES ${APP_DIR}/EMkinematical.f90 
                        LINK_LIBRARIES ${EXE_LINK_LIBRARIES}  
                        TEMPLATE ${TMPLT_DIR}/EMkinematical.template 
                        SOLUTION_FOLDER EMsoftPublic/SEM 
                        INSTALL_PROGRAM TRUE
                        INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                        )

endif()

