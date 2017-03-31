

include("${EMsoft_SOURCE_DIR}/Source/EMsoft_Functions.cmake")
set(EXE_LINK_LIBRARIES "")


if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  set(FORTRAN_LIBRARIES gomp gcc_eh)
endif()


include_directories("${EMsoftHDFLib_BINARY_DIR}")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/DictionaryIndexing")
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
  
  Add_EMsoft_Executable(TARGET EMEBSDDIstatic
                      SOURCES ${APP_DIR}/EMEBSDDIstatic.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                      TEMPLATE ${TMPLT_DIR}/EMEBSDDI.template 
                      SOLUTION_FOLDER EMsoftPublic/Dictionaryindexing
                      INSTALL_PROGRAM TRUE)
  
endif()
