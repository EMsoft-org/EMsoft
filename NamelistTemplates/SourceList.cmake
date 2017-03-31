
#---------------------------------------------------------------------
# Set some variables to shorten up the call to the function below
set(APP_DIR ${EMsoft_SOURCE_DIR}/NamelistTemplates)

#---------------------------------------------------------------------
# Aggregate all the OpenCL files that are needed
set(EMSoft_RESOURCE_FILES
  ${APP_DIR}/BetheParameters.template
  ${APP_DIR}/EMAverageOrient.template
  ${APP_DIR}/EMDisorientations.template
  ${APP_DIR}/EMConvertOrientations.template
  ${APP_DIR}/EMDPFit.template
  ${APP_DIR}/EMEBSD.template
  ${APP_DIR}/EMEBSDDI.template
  ${APP_DIR}/EMEBSDFull.template
  ${APP_DIR}/EMEBSDmaster.template
  ${APP_DIR}/EMECCI.template
  ${APP_DIR}/EMECP.template
  ${APP_DIR}/EMECPDI.template
  ${APP_DIR}/EMECPSingle.template
  ${APP_DIR}/EMECPZA.template
  ${APP_DIR}/EMECPmaster.template
  ${APP_DIR}/EMKAM.template
  ${APP_DIR}/EMKosselmaster.template
  ${APP_DIR}/EMkinematical.template
  ${APP_DIR}/EMMC.template
  ${APP_DIR}/EMMCOpenCL.template
  ${APP_DIR}/EMMCfoil.template
  ${APP_DIR}/EMOrientationSimilarity.template
  ${APP_DIR}/EMOrientationViz.template
  ${APP_DIR}/EMPEDZA.template
  ${APP_DIR}/EMTKDmaster.template
  ${APP_DIR}/EMdefectdata.template
  ${APP_DIR}/EMdefects.template
  ${APP_DIR}/EMfoil.template
  ${APP_DIR}/EMsampleRFZ.template
)

file(COPY "${EMsoft_SOURCE_DIR}/NamelistTemplates" DESTINATION "${PROJECT_BINARY_DIR}/")


#---------------------------------------------------------------------
# Create the Installation Rules
INSTALL(FILES ${EMSoft_RESOURCE_FILES}
  COMPONENT Applications
  DESTINATION "NamelistTemplates"
)
