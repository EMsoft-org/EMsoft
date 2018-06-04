include("${CMAKE_CURRENT_LIST_DIR}/EMSoftLibTargets.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/EMSoftLibHDFTargets.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/EMSoftWrapperLibTargets.cmake")

set(EMsoft_INCLUDE_DIRS "${CMAKE_CURRENT_LIST_DIR}/../../../include")
set(EMsoft_LIB_DIRS "${CMAKE_CURRENT_LIST_DIR}/../../../lib;${CMAKE_CURRENT_LIST_DIR}/../../../bin")

