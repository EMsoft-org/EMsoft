set(EMsoftWorkbench_AngleWidgets_SOURCE_DIR ${EMsoftWorkbench_SOURCE_DIR}/Source/EMsoftWorkbench)


set(Angle_Widgets_HDRS "")
set(Angle_Widgets_MOC_HDRS "")
set(Angle_Widgets_SRCS "")
set(Angle_Widgets_UIS "")

set(Angle_Widgets_MOC_HDRS ${Angle_Widgets_MOC_HDRS}
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/AbstractAngleWidget.h
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/AngleReaderWidget.h
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/SampleCubochoricSpaceWidget.h
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/SamplingRateWidget.h
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/SingleAngleWidget.h
  )

set(Angle_Widgets_SRCS ${Angle_Widgets_SRCS}
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/AbstractAngleWidget.cpp
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/AngleReaderWidget.cpp
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/SampleCubochoricSpaceWidget.cpp
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/SamplingRateWidget.cpp
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/SingleAngleWidget.cpp
    )

set(Angle_Widgets_UIS ${Angle_Widgets_UIS}
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/UI_Files/AngleReaderWidget.ui
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/UI_Files/SampleCubochoricSpaceWidget.ui
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/UI_Files/SamplingRateWidget.ui
    ${EMsoftWorkbench_AngleWidgets_SOURCE_DIR}/AngleWidgets/UI_Files/SingleAngleWidget.ui
    )

# Add in the remaining sources that are actually widgets but are completely Custom and do NOT use private
# inheritance through a .ui file
set(Angle_Widgets_HDRS ${Angle_Widgets_HDRS}

    )

set(Angle_Widgets_HDRS ${Angle_Widgets_HDRS}
      ${${PLUGIN_NAME}_Widgets_MOC_HDRS}
  )

# Organize the Source files for things like Visual Studio and Xcode
cmp_IDE_SOURCE_PROPERTIES( "AngleWidgets" "${Angle_Widgets_HDRS}" "${Angle_Widgets_SRCS}" "0")

# Organize the Source files for things like Visual Studio and Xcode
cmp_IDE_GENERATED_PROPERTIES("AngleWidgets/UI_Files" "${Angle_Widgets_UIS}" "")

QT5_WRAP_CPP( Angle_Widgets_Generated_MOC_SRCS ${Angle_Widgets_MOC_HDRS} )

# --------------------------------------------------------------------
# We are using CMake's AuotMoc feature so we do not need to wrap our .cpp files with a specific call to 'moc'

# These generated moc files will be #include in the FilterWidget source file that
# are generated so we need to tell the build system to NOT compile these files
# set_source_files_properties( ${${PLUGIN_NAME}_Widgets_Generated_MOC_SRCS} PROPERTIES HEADER_FILE_ONLY TRUE)

# --------------------------------------------------------------------
# -- Run UIC on the necessary files
QT5_WRAP_UI( Angle_Widgets_Generated_UI_HDRS ${Angle_Widgets_UIS} )

# --------------------------------------------------------------------
#-- Put the Qt generated files into their own group for IDEs
cmp_IDE_SOURCE_PROPERTIES( "Generated/Qt_Moc" "" "${Angle_Widgets_Generated_MOC_SRCS}" "0")
cmp_IDE_SOURCE_PROPERTIES( "Generated/Qt_Uic" "${Angle_Widgets_Generated_UI_HDRS}" "" "0")

# --------------------------------------------------------------------
# If you are doing more advanced Qt programming where you are including resources you will have to enable this section
# with your own cmake codes to include your resource file (.qrc) and any other needed files
# QT5_ADD_RESOURCES( ${PLUGIN_NAME}_Generated_RC_SRCS ""  )
# cmp_IDE_SOURCE_PROPERTIES( "Generated/Qt_Qrc" "${${PLUGIN_NAME}_Generated_RC_SRCS}" "" "0")




