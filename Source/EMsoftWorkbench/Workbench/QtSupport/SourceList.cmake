
set(QtSupport_HDRS
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSFileUtils.h
)

set(QtSupport_MOC_HDRS
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSFileCompleter.h
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSLineEdit.h
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSRecentFileList.h
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSSettings.h
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSStringEdit.h
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/SVControlWidgets.h
  
)

set(QtSupport_SRCS
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSFileCompleter.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSLineEdit.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSFileUtils.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSRecentFileList.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSSettings.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/QtSStringEdit.cpp
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/SVControlWidgets.cpp
)

QT5_WRAP_CPP( QtSupport_MOC_HDRS_Generated_MOC_SRCS ${QtSupport_MOC_HDRS_MOC_HDRS} )
set_source_files_properties( ${QtSupport_MOC_HDRS_Generated_MOC_SRCS} PROPERTIES GENERATED TRUE)

# --------------------------------------------------------------------
# Wrap UI files so they are AUTO UIC'ed
QT5_WRAP_UI( SVWidgetsLib_QtSupport_Generated_UI_HDRS
  ${EMsoftWorkbench_SOURCE_DIR}/QtSupport/UI_Files/QtSStringEdit.ui
)
set_source_files_properties( ${SVWidgetsLib_QtSupport_Generated_UI_HDRS} PROPERTIES GENERATED TRUE)
