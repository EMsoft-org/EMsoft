

# for now (Release 5.0), the python wrapper build is only done for Mac OS X or Linux
if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/pyEMsoft")
  set(USERHOMEDIR "$ENV{HOME}")
  get_filename_component(EMsoft_BUILD_DIR ${EMsoft_BINARY_DIR} DIRECTORY)
  #-------------------------------------------------------------------------------
  # add run_pyEMsoft.sh to Build folder with properly substituted folder locations
  Message(STATUS "Configuring run_pyEMsoft.sh script in ${EMsoft_BUILD_DIR} folder")
  cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${EMsoft_BUILD_DIR}/run_pyEMsoft.sh
                                CONFIGURED_TEMPLATE_PATH   ${APP_DIR}/run_pyEMsoft.sh.in  )
  cmpConfigureFileWithMD5Check( GENERATED_FILE_PATH        ${EMsoft_BUILD_DIR}/run_docs.sh
                                CONFIGURED_TEMPLATE_PATH   ${APP_DIR}/run_docs.sh.in  )
endif()

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
  if(WIN32)
    Message(STATUS "Python wrapper script is currently not available on Windows")
  endif()
endif()

