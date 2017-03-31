
#---------------------------------------------------------------------
# Set some variables to shorten up the call to the function below
set(APP_DIR ${EMsoft_SOURCE_DIR}/manuals/examples/KosselPatterns)

#---------------------------------------------------------------------
# Aggregate all the OpenCL files that are needed
set(EMSoft_DefectSim_FILES
  ${APP_DIR}/Ex5.nml
)

#---------------------------------------------------------------------
# Create the Installation Rules
INSTALL(FILES ${EMSoft_DefectSim_FILES}
  COMPONENT Applications
  DESTINATION "examples/KosselPatterns"
)
