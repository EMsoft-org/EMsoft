set(SUBDIR_NAME Modules)

set(${SUBDIR_NAME}_DIR "${EMsoftWorkbench_SOURCE_DIR}/${SUBDIR_NAME}")

# --------------------------------------------------------------------
# Any Class that inherits from QObject, either directly or through the heirarchy needs to have its header listed here
set(EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS
  ${${SUBDIR_NAME}_DIR}/IModuleUI.h
  ${${SUBDIR_NAME}_DIR}/IWorkbenchModule.hpp
)

# --------------------------------------------------------------------
# Run Qts automoc program to generate some source files that get compiled
QT5_WRAP_CPP( EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS ${EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS})
set_source_files_properties( ${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS} PROPERTIES GENERATED TRUE)
#set_source_files_properties( ${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS} PROPERTIES HEADER_FILE_ONLY TRUE)

set(EMsoftWorkbench_${SUBDIR_NAME}_HDRS
  ${${SUBDIR_NAME}_DIR}/IModuleFactory.hpp
  ${${SUBDIR_NAME}_DIR}/ModuleFactory.hpp
  ${${SUBDIR_NAME}_DIR}/ModuleManager.h
  ${${SUBDIR_NAME}_DIR}/cl.hpp
  ${${SUBDIR_NAME}_DIR}/ModuleTools.hpp
)


set(EMsoftWorkbench_${SUBDIR_NAME}_SRCS
  ${${SUBDIR_NAME}_DIR}/IModuleUI.cpp
  ${${SUBDIR_NAME}_DIR}/ModuleManager.cpp
)

set(EMsoftWorkbench_${SUBDIR_NAME}_HDRS
  ${EMsoftWorkbench_${SUBDIR_NAME}_HDRS}
  ${EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS}  # Add the headers that get Moc'ed here so they show up in solutions/IDEs/Project files
)
cmp_IDE_SOURCE_PROPERTIES( "${SUBDIR_NAME}" "${EMsoftWorkbench_${SUBDIR_NAME}_HDRS};${EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS}" "${EMsoftWorkbench_${SUBDIR_NAME}_SRCS}" "${PROJECT_INSTALL_HEADERS}")
cmp_IDE_GENERATED_PROPERTIES( "Generated/Qt_Moc" "" "${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS}" "0")

set(EMsoftWorkbench_${SUBDIR_NAME}_SRCS
  ${EMsoftWorkbench_${SUBDIR_NAME}_SRCS}
  ${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS}
)



# --------------------------------------------------------------------
# Include all of our modules
include(${${SUBDIR_NAME}_DIR}/PatternDisplayModule/SourceList.cmake)
include(${${SUBDIR_NAME}_DIR}/PatternFitModule/SourceList.cmake)
include(${${SUBDIR_NAME}_DIR}/CrystalStructureCreationModule/SourceList.cmake)
include(${${SUBDIR_NAME}_DIR}/MonteCarloSimulationModule/SourceList.cmake)
include(${${SUBDIR_NAME}_DIR}/MasterPatternSimulationModule/SourceList.cmake)
include(${${SUBDIR_NAME}_DIR}/DictionaryIndexingModule/SourceList.cmake)
include(${${SUBDIR_NAME}_DIR}/DictionaryIndexingModule/SourceList.cmake)

# -- Add the binary directory for this subdirectory to the include path which is where the moc files are generated
include_directories( ${EMsoftWorkbench_BINARY_DIR})

set(EMsoftWorkbench_${SUBDIR_NAME}_SOURCES
${EMsoftWorkbench_${SUBDIR_NAME}_Moc_HDRS}
${EMsoftWorkbench_${SUBDIR_NAME}_HDRS}
${EMsoftWorkbench_${SUBDIR_NAME}_SRCS}
${EMsoftWorkbench_${SUBDIR_NAME}_Generated_MOC_SRCS}
)

