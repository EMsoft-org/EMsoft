
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

if(NOT EXISTS "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources")
  file(MAKE_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources")
endif()

foreach(file ${EMSoft_RESOURCE_FILES})
  file(COPY "${file}" DESTINATION "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources")
endforeach()


#---------------------------------------------------------------------
# This sets up the two variables install_dir and lib_install_dir
EMsoft_SetupInstallDirs()

#---------------------------------------------------------------------
# Create the Installation Rules
INSTALL(FILES ${EMSoft_RESOURCE_FILES}
  COMPONENT Applications
  DESTINATION ${extra_install_dir}/resources
)

