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

set(MODULE_NAME DictionaryIndexingModule)

set(${MODULE_NAME}_DIR "${${SUBDIR_NAME}_DIR}/${MODULE_NAME}")

include_directories(${${MODULE_NAME}_DIR})

# --------------------------------------------------------------------
# Any Class that inherits from QObject, either directly or through the heirarchy needs to have its header listed here
set(EMsoftWorkbench_${MODULE_NAME}_Moc_HDRS
  ${${MODULE_NAME}_DIR}/ADPMapImageViewer.h
  ${${MODULE_NAME}_DIR}/ADPMap_UI.h
  ${${MODULE_NAME}_DIR}/DictionaryIndexing_UI.h
  ${${MODULE_NAME}_DIR}/DictionaryIndexingMain_UI.h
  ${${MODULE_NAME}_DIR}/ADPMapController.h
  ${${MODULE_NAME}_DIR}/DIImageViewer.h
  ${${MODULE_NAME}_DIR}/DictionaryIndexingModule.h
  ${${MODULE_NAME}_DIR}/DictionaryIndexingController.h
  ${${MODULE_NAME}_DIR}/ChoosePatternsDataset_UI.h
  ${${MODULE_NAME}_DIR}/PatternPreprocessing_UI.h
  ${${MODULE_NAME}_DIR}/PatternPreprocessingController.h
  ${${MODULE_NAME}_DIR}/PPMatrixImageViewer.h
)

# --------------------------------------------------------------------
# Run Qts automoc program to generate some source files that get compiled
QT5_WRAP_CPP( EMsoftWorkbench_${MODULE_NAME}_Generated_MOC_SRCS ${EMsoftWorkbench_${MODULE_NAME}_Moc_HDRS})
set_source_files_properties( ${EMsoftWorkbench_${MODULE_NAME}_Generated_MOC_SRCS} PROPERTIES GENERATED TRUE)
#set_source_files_properties( ${EMsoftWorkbench_${MODULE_NAME}_Generated_MOC_SRCS} PROPERTIES HEADER_FILE_ONLY TRUE)

set(EMsoftWorkbench_${MODULE_NAME}_HDRS
  ${${MODULE_NAME}_DIR}/Constants.h
)


set(EMsoftWorkbench_${MODULE_NAME}_SRCS
  ${${MODULE_NAME}_DIR}/ADPMapImageViewer.cpp
  ${${MODULE_NAME}_DIR}/ADPMap_UI.cpp
  ${${MODULE_NAME}_DIR}/DictionaryIndexing_UI.cpp
  ${${MODULE_NAME}_DIR}/DictionaryIndexingMain_UI.cpp
  ${${MODULE_NAME}_DIR}/ADPMapController.cpp
  ${${MODULE_NAME}_DIR}/DIImageViewer.cpp
  ${${MODULE_NAME}_DIR}/DictionaryIndexingModule.cpp
  ${${MODULE_NAME}_DIR}/DictionaryIndexingController.cpp
  ${${MODULE_NAME}_DIR}/ChoosePatternsDataset_UI.cpp
  ${${MODULE_NAME}_DIR}/PatternPreprocessing_UI.cpp
  ${${MODULE_NAME}_DIR}/PatternPreprocessingController.cpp
  ${${MODULE_NAME}_DIR}/PPMatrixImageViewer.cpp
)

set(EMsoftWorkbench_${MODULE_NAME}_UIS
  ${${MODULE_NAME}_DIR}/UI_Files/ADPMap_UI.ui
  ${${MODULE_NAME}_DIR}/UI_Files/DictionaryIndexing_UI.ui
  ${${MODULE_NAME}_DIR}/UI_Files/DictionaryIndexingMain_UI.ui
  ${${MODULE_NAME}_DIR}/UI_Files/ChoosePatternsDataset_UI.ui
  ${${MODULE_NAME}_DIR}/UI_Files/PatternPreprocessing_UI.ui
  )
# --------------------------------------------------------------------
# Continue on with our Qt4 section
QT5_WRAP_UI( EMsoftWorkbench_${MODULE_NAME}_Generated_UI_HDRS   
  ${EMsoftWorkbench_${MODULE_NAME}_UIS}
)

cmp_IDE_SOURCE_PROPERTIES( "Modules/${MODULE_NAME}/UI_Files" "${EMsoftWorkbench_${MODULE_NAME}_UIS}" "" "${PROJECT_INSTALL_HEADERS}")

set(EMsoftWorkbench_${MODULE_NAME}_HDRS
  ${EMsoftWorkbench_${MODULE_NAME}_HDRS}
  ${EMsoftWorkbench_${MODULE_NAME}_Moc_HDRS}  # Add the headers that get Moc'ed here so they show up in solutions/IDEs/Project files
)



cmp_IDE_SOURCE_PROPERTIES( "Modules/${MODULE_NAME}" "${EMsoftWorkbench_${MODULE_NAME}_HDRS};${EMsoftWorkbench_${MODULE_NAME}_Moc_HDRS}" "${EMsoftWorkbench_${MODULE_NAME}_SRCS}" "${PROJECT_INSTALL_HEADERS}")
cmp_IDE_GENERATED_PROPERTIES( "Generated/${MODULE_NAME}" "" "${EMsoftWorkbench_${MODULE_NAME}_Generated_MOC_SRCS}" "0")
cmp_IDE_GENERATED_PROPERTIES( "Generated/${MODULE_NAME}/Qt_Uic" "${EMsoftWorkbench_${MODULE_NAME}_Generated_UI_HDRS}" "" "0")


set(EMsoftWorkbench_${MODULE_NAME}_SRCS
  ${EMsoftWorkbench_${MODULE_NAME}_SRCS}
  ${EMsoftWorkbench_${MODULE_NAME}_Generated_MOC_SRCS}
  )

# -- Add the binary directory for this subdirectory to the include path which is where the moc files are generated
include_directories( ${EMsoftWorkbench_BINARY_DIR})

set(EMsoftWorkbench_${MODULE_NAME}_SOURCES
${EMsoftWorkbench_${MODULE_NAME}_Moc_HDRS}
${EMsoftWorkbench_${MODULE_NAME}_HDRS}
${EMsoftWorkbench_${MODULE_NAME}_SRCS}
${EMsoftWorkbench_${MODULE_NAME}_Generated_MOC_SRCS}
${EMsoftWorkbench_${MODULE_NAME}_Generated_UI_HDRS}
)
