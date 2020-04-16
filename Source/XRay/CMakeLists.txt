set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/XRay")
set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})


if(EMsoft_ENABLE_HDF5_SUPPORT)

  GetHDF5LinkLibraries(EMSOFT)
  set(EXE_LINK_LIBRARIES ${EMSOFT_hdf5LinkLibs} EMsoftHDFLib)

  
  Add_EMsoft_Executable(TARGET PFInversion 
                      SOURCES ${APP_DIR}/PFInversion.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}  
                      TEMPLATE ${TMPLT_DIR}/PFInversion.template
                      SOLUTION_FOLDER EMsoftPublic/XRay
                      INSTALL_PROGRAM TRUE
                      INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                      )


  Add_EMsoft_Executable(TARGET EMLauemaster 
                      SOURCES ${APP_DIR}/EMLauemaster.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}  
                      TEMPLATE ${TMPLT_DIR}/EMLauemaster.template
                      SOLUTION_FOLDER EMsoftPublic/XRay
                      INSTALL_PROGRAM TRUE
                      INCLUDE_DIRS ${EMsoftHDFLib_BINARY_DIR}
                      )

  Add_EMsoft_Executable(TARGET EMLaue
                      SOURCES ${APP_DIR}/EMLaue.f90
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}  
                      TEMPLATE ${TMPLT_DIR}/EMLaue.template
                      SOLUTION_FOLDER EMsoftPublic/XRay
                      INSTALL_PROGRAM TRUE
                      INCLUDE_DIRS ${EMsoftLib_BINARY_DIR} ${EMsoftHDFLib_BINARY_DIR} ${EMsoft_BINARY_DIR}/EMsoftLib
                      )

  Add_EMsoft_Executable(TARGET EMLaueSlit
                      SOURCES ${APP_DIR}/EMLaueSlit.f90
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}  
                      TEMPLATE ${TMPLT_DIR}/EMLaue.template
                      SOLUTION_FOLDER EMsoftPublic/XRay
                      INSTALL_PROGRAM TRUE
                      INCLUDE_DIRS ${EMsoftLib_BINARY_DIR} ${EMsoftHDFLib_BINARY_DIR} ${EMsoft_BINARY_DIR}/EMsoftLib
                      )

endif()

