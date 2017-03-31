
#---------------------------------------------------------------------
# Set some variables to shorten up the call to the function below
set(APP_DIR ${EMsoft_SOURCE_DIR}/resources)

#---------------------------------------------------------------------
# Aggregate all the files that are needed
set(EMSoft_RESOURCE_FILES
#  ${APP_DIR}/rotations.txt
  ${APP_DIR}/templatecodes.txt
  ${APP_DIR}/RandomSeeds.data
  ${APP_DIR}/EBSDview.data
  ${APP_DIR}/Schematic1.data
  ${APP_DIR}/Schematic2.data
  ${APP_DIR}/Schematic3.data
  ${APP_DIR}/Schematic4.data
  ${APP_DIR}/WyckoffPositions.txt
  ${APP_DIR}/rotations.txt
)

#---------------------------------------------------------------------
# Create the Installation Rules
INSTALL(FILES ${EMSoft_RESOURCE_FILES}
  COMPONENT Applications
  DESTINATION "resources"
)
