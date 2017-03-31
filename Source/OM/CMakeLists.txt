

include("${EMsoft_SOURCE_DIR}/Source/EMsoft_Functions.cmake")
include_directories("${EMsoftHDFLib_BINARY_DIR}")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
set(APP_DIR "${EMsoft_SOURCE_DIR}/Source/OM")
set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")
