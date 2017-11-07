
#---------------------------------------------------------------------
# Set some variables to shorten up the call to the function below
set(APP_DIR ${EMsoft_SOURCE_DIR}/manuals/examples/EBSDPatterns)

#---------------------------------------------------------------------
# Aggregate all the OpenCL files that are needed
set(EMSoft_DefectSim_FILES
  ${APP_DIR}/BetheParameters.nml
  ${APP_DIR}/EMEBSDexample.nml
  ${APP_DIR}/EMEBSDMexample.nml
  ${APP_DIR}/EMMCexample.nml
)

#---------------------------------------------------------------------
# This sets up the two variables install_dir and lib_install_dir
EMsoft_SetupInstallDirs()

#---------------------------------------------------------------------
# Create the Installation Rules
INSTALL(FILES ${EMSoft_DefectSim_FILES}
  COMPONENT Applications
  DESTINATION "${top_install_dir}examples/EBSDPatterns"
)
