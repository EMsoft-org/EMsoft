
#---------------------------------------------------------------------
# Set some variables to shorten up the call to the function below
set(APP_DIR ${EMsoft_SOURCE_DIR}/NamelistTemplates)

#---------------------------------------------------------------------
# Aggregate all the OpenCL files that are needed
set(EMSoft_RESOURCE_FILES
  ${APP_DIR}/BetheParameters.template
  ${APP_DIR}/EMAverageOrient.template
  ${APP_DIR}/EMConvertOrientations.template
  ${APP_DIR}/EMDPFit.template
  ${APP_DIR}/EMEBSD.template
  ${APP_DIR}/EMEBSDDI.template
  ${APP_DIR}/EMEBSDDIpreview.template
  ${APP_DIR}/EMEBSDDIchangesetting.template
  ${APP_DIR}/EMEBSDFull.template
  ${APP_DIR}/EMEBSDmaster.template
  ${APP_DIR}/EMECCI.template
  ${APP_DIR}/EMECP.template
  ${APP_DIR}/EMECPDI.template
  ${APP_DIR}/EMECPSingle.template
  ${APP_DIR}/EMECPZA.template
  ${APP_DIR}/EMECPmaster.template
  ${APP_DIR}/EMgetADP.template
  ${APP_DIR}/EMgetOSM.template
  ${APP_DIR}/EMgetANG.template
  ${APP_DIR}/EMgetCTF.template
  ${APP_DIR}/EMGBO.template
  ${APP_DIR}/EMKAM.template
  ${APP_DIR}/EMKosselmaster.template
  ${APP_DIR}/EMLACBED.template
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
  ${APP_DIR}/EMoSLERP.template
  ${APP_DIR}/EMfoil.template
  ${APP_DIR}/EMsampleRFZ.template
  ${APP_DIR}/EMFitOrientation.template
  ${APP_DIR}/MDElectronProp.template
  ${APP_DIR}/EMTKDmaster.template
  ${APP_DIR}/EMTKD.template
  ${APP_DIR}/PFInversion.template
  ${APP_DIR}/EMTKDDI.template
  ${APP_DIR}/EMCBEDQC.template
  ${APP_DIR}/EMCBED2DQC.template
  ${APP_DIR}/EMEBSD2DQCmaster.template
  ${APP_DIR}/EMEBSDQCmaster.template
  ${APP_DIR}/EMECPQCmaster.template
  ${APP_DIR}/EMLauemaster.template
  ${APP_DIR}/EMLaue.template
  ${APP_DIR}/EMSphInx.template
  ${APP_DIR}/EMEBSDoverlap.template
  ${APP_DIR}/EMCPLMmaster.template
)

if(NOT EXISTS "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/NamelistTemplates")
  file(MAKE_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/NamelistTemplates")
endif()

foreach(file ${EMSoft_RESOURCE_FILES})
  file(COPY "${file}" DESTINATION "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/NamelistTemplates")
endforeach()

#---------------------------------------------------------------------
# This sets up the two variables install_dir and lib_install_dir
EMsoft_SetupInstallDirs()

#---------------------------------------------------------------------
# Create the Installation Rules
INSTALL(FILES ${EMSoft_RESOURCE_FILES}
  COMPONENT Applications
  DESTINATION ${extra_install_dir}/NamelistTemplates
)
