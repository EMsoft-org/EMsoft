

set(SUBDIR_NAME Common)

set(${SUBDIR_NAME}_DIR "${EMsoftWorkbench_SOURCE_DIR}/${SUBDIR_NAME}")

# --------------------------------------------------------------------
# Any Class that inherits from QObject, either directly or through the heirarchy needs to have its header listed here
set(EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS
  ${${SUBDIR_NAME}_DIR}/GLImageViewer.h
  ${${SUBDIR_NAME}_DIR}/PatternImageViewer.h
  ${${SUBDIR_NAME}_DIR}/MonteCarloFileReader.h
  ${${SUBDIR_NAME}_DIR}/ProjectionConversions.hpp
  #${${SUBDIR_NAME}_DIR}/SVStyle.h
  ${${SUBDIR_NAME}_DIR}/XtalFileReader.h
  ${${SUBDIR_NAME}_DIR}/EMsoftFileWriter.h
  ${${SUBDIR_NAME}_DIR}/HDF5DatasetSelectionWidget.h
  ${${SUBDIR_NAME}_DIR}/HDF5FileTreeModel.h
  # ${${SUBDIR_NAME}_DIR}/SVControlWidgets.h
)

# --------------------------------------------------------------------
# Run Qts automoc program to generate some source files that get compiled
QT5_WRAP_CPP( EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS ${EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS})
set_source_files_properties( ${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS} PROPERTIES GENERATED TRUE)
#set_source_files_properties( ${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS} PROPERTIES HEADER_FILE_ONLY TRUE)

set(EMsoftWorkbench_${SUBDIR_NAME}_HDRS
  ${${SUBDIR_NAME}_DIR}/AbstractImageGenerator.hpp
  ${${SUBDIR_NAME}_DIR}/Constants.h
  ${${SUBDIR_NAME}_DIR}/EigenConversions.hpp
  ${${SUBDIR_NAME}_DIR}/FileIOTools.h
  ${${SUBDIR_NAME}_DIR}/HDF5FileTreeModelItem.h
  ${${SUBDIR_NAME}_DIR}/ImageGenerationTask.hpp
  ${${SUBDIR_NAME}_DIR}/ImageGenerator.hpp
  ${${SUBDIR_NAME}_DIR}/IObserver.h
  ${${SUBDIR_NAME}_DIR}/MasterPatternFileReader.h
  ${${SUBDIR_NAME}_DIR}/PatternTools.h
  ${${SUBDIR_NAME}_DIR}/ProjectionConversionTask.hpp
)


set(EMsoftWorkbench_${SUBDIR_NAME}_SRCS
  ${${SUBDIR_NAME}_DIR}/FileIOTools.cpp
  ${${SUBDIR_NAME}_DIR}/GLImageViewer.cpp
  ${${SUBDIR_NAME}_DIR}/PatternImageViewer.cpp
  ${${SUBDIR_NAME}_DIR}/IObserver.cpp
  ${${SUBDIR_NAME}_DIR}/MasterPatternFileReader.cpp
  ${${SUBDIR_NAME}_DIR}/MonteCarloFileReader.cpp
  ${${SUBDIR_NAME}_DIR}/PatternTools.cpp
  #${${SUBDIR_NAME}_DIR}/SVStyle.cpp
  ${${SUBDIR_NAME}_DIR}/XtalFileReader.cpp
  ${${SUBDIR_NAME}_DIR}/EMsoftFileWriter.cpp
  ${${SUBDIR_NAME}_DIR}/HDF5DatasetSelectionWidget.cpp
  ${${SUBDIR_NAME}_DIR}/HDF5FileTreeModel.cpp
  ${${SUBDIR_NAME}_DIR}/HDF5FileTreeModelItem.cpp
  #${${SUBDIR_NAME}_DIR}/SVControlWidgets.cpp
)

set(EMsoftWorkbench_${SUBDIR_NAME}_UIS
  ${${SUBDIR_NAME}_DIR}/UI_Files/HDF5DatasetSelectionWidget.ui
  )

# --------------------------------------------------------------------
# Continue on with our Qt4 section
QT5_WRAP_UI( EMsoftWorkbench_${SUBDIR_NAME}_Generated_UI_HDRS   
${EMsoftWorkbench_${SUBDIR_NAME}_UIS}
)

cmp_IDE_SOURCE_PROPERTIES( "EMsoftWorkbench/UI_Files" "${EMsoftWorkbench_${SUBDIR_NAME}_UIS}" "" "${PROJECT_INSTALL_HEADERS}")
cmp_IDE_SOURCE_PROPERTIES( "${SUBDIR_NAME}" "${EMsoftWorkbench_${SUBDIR_NAME}_HDRS};${EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS}" "${EMsoftWorkbench_${SUBDIR_NAME}_SRCS}" "${PROJECT_INSTALL_HEADERS}")
cmp_IDE_GENERATED_PROPERTIES( "Generated/${SUBDIR_NAME}" "" "${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS}")
cmp_IDE_GENERATED_PROPERTIES( "Generated/Qt_Uic" "${EMsoftWorkbench_${SUBDIR_NAME}_Generated_UI_HDRS}" "" "0")

set(EMsoftWorkbench_${SUBDIR_NAME}_HDRS
  ${EMsoftWorkbench_${SUBDIR_NAME}_HDRS}
  ${EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS}  # Add the headers that get Moc'ed here so they show up in solutions/IDEs/Project files
)

set(EMsoftWorkbench_${SUBDIR_NAME}_SRCS
  ${EMsoftWorkbench_${SUBDIR_NAME}_SRCS}
  ${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS}
)

# -- Add the binary directory for this subdirectory to the include path which is where the moc files are generated
include_directories( ${EMsoftWorkbench_BINARY_DIR})
include_directories(${EMsoftWorkbench_SOURCE_DIR}/Source/QtSupport)

set(EMsoftWorkbench_${SUBDIR_NAME}_SOURCES
${EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS}
${EMsoftWorkbench_${SUBDIR_NAME}_HDRS}
${EMsoftWorkbench_${SUBDIR_NAME}_SRCS}
${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS}
${EMsoftWorkbench_${SUBDIR_NAME}_Generated_UI_HDRS}
)

