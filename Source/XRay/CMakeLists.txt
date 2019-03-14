
set(EXE_LINK_LIBRARIES "")

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  set(EXE_LINK_LIBRARIES ${EXE_LINK_LIBRARIES} gomp gcc_eh)
endif()

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
  if(WIN32)
    add_definitions("/Qopenmp /Qdiag-disable:11082 /Qip")
  endif()
endif()


include_directories("${EMsoftHDFLib_BINARY_DIR}")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/XRay")
set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")


if(EMsoft_ENABLE_HDF5_SUPPORT)
  
  GetHDF5LinkLibraries(EMSOFT)
  set(EXE_LINK_LIBRARIES ${EXE_LINK_LIBRARIES} ${EMSOFT_hdf5LinkLibs} EMsoftHDFLib EMsoftLib)

  
  Add_EMsoft_Executable(TARGET PFInversion 
                      SOURCES ${APP_DIR}/PFInversion.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES} ${FORTRAN_LIBRARIES} 
                      TEMPLATE ${TMPLT_DIR}/PFInversion.template
                      SOLUTION_FOLDER EMsoftPublic/XRay
                      INSTALL_PROGRAM TRUE)

  Add_EMsoft_Executable(TARGET EMLauemaster 
                      SOURCES ${APP_DIR}/EMLauemaster.f90 
                      LINK_LIBRARIES ${EXE_LINK_LIBRARIES} ${FORTRAN_LIBRARIES} 
                      TEMPLATE ${TMPLT_DIR}/EMLauemaster.template
                      SOLUTION_FOLDER EMsoftPublic/XRay
                      INSTALL_PROGRAM TRUE)

endif()

