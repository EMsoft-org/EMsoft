
#---------------------------------------------------------------------
# Set some variables to shorten up the call to the function below
set(APP_DIR ${EMsoft_SOURCE_DIR}/opencl)

#---------------------------------------------------------------------
# Aggregate all the OpenCL files that are needed
set(EMSoft_CL_SRCS
  ${APP_DIR}/DictIndx.cl
  ${APP_DIR}/EMMC.cl
  ${APP_DIR}/EMMCxyz.cl
  ${APP_DIR}/MBmoduleOpenCL.cl
)

#---------------------------------------------------------------------
# Create the Installation Rules
INSTALL(FILES ${EMSoft_CL_SRCS}
  COMPONENT Applications
  DESTINATION "opencl"
)
