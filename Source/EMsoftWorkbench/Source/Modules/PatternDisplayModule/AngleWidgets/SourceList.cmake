# ============================================================================
# Copyright (c) 2009-2015 BlueQuartz Software, LLC
#
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright notice, this
# list of conditions and the following disclaimer in the documentation and/or
# other materials provided with the distribution.
#
# Neither the name of BlueQuartz Software, the US Air Force, nor the names of its
# contributors may be used to endorse or promote products derived from this software
# without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
# USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The code contained herein was partially funded by the followig contracts:
#    United States Air Force Prime Contract FA8650-07-D-5800
#    United States Air Force Prime Contract FA8650-10-D-5210
#    United States Prime Contract Navy N00173-07-C-2068
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

set(AngleWidgets_NAME AngleWidgets)

set(${AngleWidgets_NAME}_DIR ${${MODULE_NAME}_DIR}/${AngleWidgets_NAME})


set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_HDRS "")
set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_MOC_HDRS "")
set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_SRCS "")
set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_UIS "")

set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_MOC_HDRS 
    ${Angle_Widgets_MOC_HDRS}
    ${${AngleWidgets_NAME}_DIR}/AbstractAngleWidget.h
    ${${AngleWidgets_NAME}_DIR}/AngleReaderWidget.h
    ${${AngleWidgets_NAME}_DIR}/SampleCubochoricSpaceWidget.h
    ${${AngleWidgets_NAME}_DIR}/SamplingRateWidget.h
    ${${AngleWidgets_NAME}_DIR}/SingleAngleWidget.h
  )

set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_SRCS 
    ${Angle_Widgets_SRCS}
    ${${AngleWidgets_NAME}_DIR}/AbstractAngleWidget.cpp
    ${${AngleWidgets_NAME}_DIR}/AngleReaderWidget.cpp
    ${${AngleWidgets_NAME}_DIR}/SampleCubochoricSpaceWidget.cpp
    ${${AngleWidgets_NAME}_DIR}/SamplingRateWidget.cpp
    ${${AngleWidgets_NAME}_DIR}/SingleAngleWidget.cpp
    )

set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_UIS 
    ${Angle_Widgets_UIS}
    ${${AngleWidgets_NAME}_DIR}/UI_Files/AngleReaderWidget.ui
    ${${AngleWidgets_NAME}_DIR}/UI_Files/SampleCubochoricSpaceWidget.ui
    ${${AngleWidgets_NAME}_DIR}/UI_Files/SamplingRateWidget.ui
    ${${AngleWidgets_NAME}_DIR}/UI_Files/SingleAngleWidget.ui
    )

cmp_IDE_SOURCE_PROPERTIES("Modules/${MODULE_NAME}/AngleWidgets/UI_Files" "${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_UIS}" "" 0)


# Add in the remaining sources that are actually widgets but are completely Custom and do NOT use private
# inheritance through a .ui file
set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_HDRS ${Angle_Widgets_HDRS}

    )

set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_HDRS ${Angle_Widgets_HDRS}
      ${${PLUGIN_NAME}_Widgets_MOC_HDRS}
  )

# Organize the Source files for things like Visual Studio and Xcode
cmp_IDE_SOURCE_PROPERTIES( "Modules/${MODULE_NAME}/AngleWidgets" "${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_MOC_HDRS}"
                                              "${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_SRCS}" "0")


QT5_WRAP_CPP( EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_Generated_MOC_SRCS 
            ${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_MOC_HDRS} )

# --------------------------------------------------------------------
# We are using CMake's AuotMoc feature so we do not need to wrap our .cpp files with a specific call to 'moc'

# These generated moc files will be #include in the FilterWidget source file that
# are generated so we need to tell the build system to NOT compile these files
# set_source_files_properties( ${${PLUGIN_NAME}_Widgets_Generated_MOC_SRCS} PROPERTIES HEADER_FILE_ONLY TRUE)

# --------------------------------------------------------------------
# -- Run UIC on the necessary files
QT5_WRAP_UI( EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_Generated_UI_HDRS 
    ${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_UIS} 
    )
cmp_IDE_SOURCE_PROPERTIES("Modules/${MODULE_NAME}/AngleWidgets/UI_Files" "${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_UIS}" "" 0)

# --------------------------------------------------------------------
#-- Put the Qt generated files into their own group for IDEs
cmp_IDE_GENERATED_PROPERTIES( "Generated/Qt_Moc" "" "${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_Generated_MOC_SRCS}" "0")
cmp_IDE_GENERATED_PROPERTIES( "Generated/Qt_Uic" "${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_Generated_UI_HDRS}" "" "0")

# --------------------------------------------------------------------
# If you are doing more advanced Qt programming where you are including resources you will have to enable this section
# with your own cmake codes to include your resource file (.qrc) and any other needed files
# QT5_ADD_RESOURCES( ${PLUGIN_NAME}_Generated_RC_SRCS ""  )
# cmp_IDE_SOURCE_PROPERTIES( "Generated/Qt_Qrc" "${${PLUGIN_NAME}_Generated_RC_SRCS}" "" "0")

set(EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_SOURCES
${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_Moc_HDRS}
${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_HDRS}
${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_SRCS}
${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_Generated_MOC_SRCS}
${EMsoftWorkbench_${MODULE_NAME}_${AngleWidgets_NAME}_Generated_UI_HDRS}
)


