


#---------------------------------------------------------------------
# Set some variables to shorten up the call to the function below
set(APP_DIR ${EMsoft_SOURCE_DIR}/IDL )

#---------------------------------------------------------------------
# Aggregate all the OpenCL files that are needed
set(EMSoft_IDL_PRO_FILES
${APP_DIR}/pro/Core_FitLine.pro			${APP_DIR}/pro/ECPatternWidget_event.pro
${APP_DIR}/pro/Core_LambertS2C.pro		${APP_DIR}/pro/ECPevent.pro
${APP_DIR}/pro/Core_LambertSphereToSquare.pro	${APP_DIR}/pro/ECPshowPattern.pro
${APP_DIR}/pro/Core_Lamberts2SP.pro		${APP_DIR}/pro/Efit.pro
${APP_DIR}/pro/Core_Print.pro			${APP_DIR}/pro/Efit_amoeba.pro
${APP_DIR}/pro/Core_WText.pro			${APP_DIR}/pro/Efit_control.pro
${APP_DIR}/pro/Core_WTextE.pro			${APP_DIR}/pro/Efit_control_event.pro
${APP_DIR}/pro/Core_WidgetChoiceEvent.pro	${APP_DIR}/pro/Efit_display.pro
${APP_DIR}/pro/Core_WidgetEvent.pro		${APP_DIR}/pro/Efit_display_event.pro
${APP_DIR}/pro/Core_applyaxisangle.pro		${APP_DIR}/pro/Efit_event.pro
${APP_DIR}/pro/Core_colorwheel.pro		${APP_DIR}/pro/Efit_fit.pro
${APP_DIR}/pro/Core_eu2qu.pro			${APP_DIR}/pro/Efit_navigator.pro
${APP_DIR}/pro/Core_histnd.pro			${APP_DIR}/pro/Efit_navigator_event.pro
${APP_DIR}/pro/Core_mind.pro			${APP_DIR}/pro/Efit_showpattern.pro
${APP_DIR}/pro/Core_qu2eu.pro			${APP_DIR}/pro/Efit_update.pro
${APP_DIR}/pro/Core_quat_Lp.pro			${APP_DIR}/pro/Efit_updatePC.pro
${APP_DIR}/pro/Core_quatmult.pro		${APP_DIR}/pro/Efitcalc.pro
${APP_DIR}/pro/DPADisplay_event.pro		${APP_DIR}/pro/Efitevent.pro
${APP_DIR}/pro/DPAevent.pro			${APP_DIR}/pro/Efitgetfilename.pro
${APP_DIR}/pro/DPAgetfilename.pro		${APP_DIR}/pro/Efitgetpreferences.pro
${APP_DIR}/pro/DPAloadfile.pro			${APP_DIR}/pro/Efitinit.pro
${APP_DIR}/pro/DPanalysis.pro			${APP_DIR}/pro/Efitwritepreferences.pro
${APP_DIR}/pro/EBSDDetectorWidget.pro	        ${APP_DIR}/pro/EBSDDetectorWidget_event.pro
${APP_DIR}/pro/EBSDExecute.pro			${APP_DIR}/pro/KosselDetectorWidget.pro
${APP_DIR}/pro/EBSDMCDisplayWidget.pro		${APP_DIR}/pro/KosselDetectorWidget_event.pro
${APP_DIR}/pro/EBSDMCDisplayWidget_event.pro	${APP_DIR}/pro/KosselExecute.pro
${APP_DIR}/pro/EBSDPatternWidget.pro		${APP_DIR}/pro/KosselPatternWidget.pro
${APP_DIR}/pro/EBSDPatternWidget_event.pro	${APP_DIR}/pro/KosselPatternWidget_event.pro
${APP_DIR}/pro/EBSD_updatePC.pro		${APP_DIR}/pro/Kosselevent.pro
${APP_DIR}/pro/EBSDcalc.pro			${APP_DIR}/pro/KosselshowPattern.pro
${APP_DIR}/pro/EBSDevent.pro			${APP_DIR}/pro/EBSDfit.pro			
${APP_DIR}/pro/EBSDfit_event.pro		${APP_DIR}/pro/EBSDgetfilename.pro		
${APP_DIR}/pro/EBSDgetpreferences.pro		${APP_DIR}/pro/EBSDinit.pro			
${APP_DIR}/pro/EBSDprint.pro			${APP_DIR}/pro/EBSDreadHDFdatafile.pro		
${APP_DIR}/pro/EBSDreadanglefile.pro		${APP_DIR}/pro/EBSDreaddatafile.pro		
${APP_DIR}/pro/EBSDshowMC.pro			${APP_DIR}/pro/EBSDshowPattern.pro		
${APP_DIR}/pro/EBSDwritepreferences.pro		${APP_DIR}/pro/ECCIDisplay.pro			
${APP_DIR}/pro/ECCIDisplay_event.pro		${APP_DIR}/pro/ECCIECPWidget.pro		
${APP_DIR}/pro/ECCIECPWidget_event.pro		${APP_DIR}/pro/ECCIECPshow.pro			
${APP_DIR}/pro/ECCIblur.pro			${APP_DIR}/pro/ECCIevent.pro			
${APP_DIR}/pro/ECCIgetfilename.pro		${APP_DIR}/pro/ECCIgetpreferences.pro		
${APP_DIR}/pro/ECCImageWidget.pro		${APP_DIR}/pro/ECCImageWidget_event.pro		
${APP_DIR}/pro/ECCIprint.pro			${APP_DIR}/pro/ECCIprogressbar.pro		
${APP_DIR}/pro/ECCIreaddatafile.pro		${APP_DIR}/pro/ECCIwritepreferences.pro		
${APP_DIR}/pro/ECPDetectorWidget.pro		${APP_DIR}/pro/SEMDisplay.pro
${APP_DIR}/pro/ECPDetectorWidget_event.pro	${APP_DIR}/pro/SEMDisplay_event.pro
${APP_DIR}/pro/ECPExecute.pro			${APP_DIR}/pro/core_getenv.pro
${APP_DIR}/pro/ECPatternWidget.pro	
)

#---------------------------------------------------------------------
# Create the Installation Rules
INSTALL(FILES ${EMSoft_IDL_PRO_FILES}
  COMPONENT Applications
  DESTINATION "IDL/pro"
)

set(EMSoft_IDL_RESOURCE_FILES
  ${APP_DIR}/Resources/CTEMlogo.jpg
  ${APP_DIR}/Resources/SEMONRlogo.jpg
  ${APP_DIR}/Resources/SEMlogo.jpg
  ${APP_DIR}/Resources/EMsoftlogo.jpg
)


#---------------------------------------------------------------------
# Create the Installation Rules
INSTALL(FILES ${EMSoft_IDL_RESOURCE_FILES}
  COMPONENT Applications
  DESTINATION "IDL/Resources"
)


#---------------------------------------------------------------------
# Make sure the IDL VMapps folder exists. At some point it will be
# filled with files.
if(NOT EXISTS "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/IDL/VMapps/SEMDisplay")
  file(MAKE_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/IDL/VMapps/SEMDisplay")
endif()

#---------------------------------------------------------------------
# Create the Installation Rules for the IDL VM
install(DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/IDL"
  COMPONENT Applications
  DESTINATION "."
  USE_SOURCE_PERMISSIONS
)

configure_file("${APP_DIR}/Resources/VMappsInstall.cmake.in"
        "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/VMappsInstall.cmake" @ONLY IMMEDIATE)

install(SCRIPT "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/VMappsInstall.cmake" COMPONENT Applications)



