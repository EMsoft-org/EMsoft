include("${CMAKE_CURRENT_LIST_DIR}/EMsoftLibTargets.cmake")


if(@EMsoft_ENABLE_HDF5_SUPPORT@)
  include("${CMAKE_CURRENT_LIST_DIR}/EMsoftHDFLibTargets.cmake")
endif()

set(EMsoft_INCLUDE_DIRS "${CMAKE_CURRENT_LIST_DIR}/../../../include")
set(EMsoft_LIB_DIRS "${CMAKE_CURRENT_LIST_DIR}/../../../lib;${CMAKE_CURRENT_LIST_DIR}/../../../bin")

set(EMsoft_Fortran_COMPILER_NAME @Fortran_COMPILER_NAME@)
set(EMsoft_BUILD_SHARED_LIBS "@BUILD_SHARED_LIBS@")

if (EMsoft_Fortran_COMPILER_NAME MATCHES "gfortran.*")
  set(EMsoft_Fortran_RT_Libs gfortran @EMsoft_FORTRAN_SUPPORT_LIBS@)
  set(EMsoft_Fortran_Lib_Dir @GFortran_LIB_DIR@)
endif()
