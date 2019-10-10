

#----------------------------------------------------------------
# 
#
function(AddIFortCopyInstallRules)
  set(options )
  set(oneValueArgs LIBNAME LIBPREFIX LIBPATH)
  set(multiValueArgs TYPES)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(INTER_DIR "")

  # message(STATUS "Z_LIBNAME: ${Z_LIBNAME}")
  # message(STATUS "Z_LIBPREFIX: ${Z_LIBPREFIX}")
  # message(STATUS "Z_LIBPATH: ${Z_LIBPATH}")
  # message(STATUS "Z_TYPES: ${Z_TYPES}")

  set(Z_INSTALL_DIR "lib")
  if(WIN32)
    set(Z_INSTALL_DIR "bin")
  endif()

  FOREACH(BTYPE ${Z_TYPES} )
    #message(STATUS "  BTYPE: ${BTYPE}")
    STRING(TOUPPER ${BTYPE} TYPE)
    if(MSVC_IDE)
      set(INTER_DIR "${BTYPE}/")
    endif()
    set(DEBUG_SUFFIX "")
    if( "${BTYPE}" STREQUAL "Debug")
        set(DEBUG_SUFFIX "d")
    endif()

    set(fullPath "${Z_LIBPATH}/${Z_LIBPREFIX}${Z_LIBNAME}${DEBUG_SUFFIX}.dll")

    # Get the Actual Library Path and create Install and copy rules
    #message(STATUS "  fullPath: ${fullPath}")
    if(NOT "${fullPath}" STREQUAL "LibPath-NOTFOUND")
      if(NOT TARGET ZZ_${Z_LIBNAME}_DLL_${TYPE}-Copy)
        #message(STATUS "Creating Install And Copy Rule for ${fullPath}")
        install(FILES ${fullPath}
          DESTINATION "${Z_INSTALL_DIR}"
          CONFIGURATIONS ${BTYPE}
          COMPONENT Applications)

        if(NOT EXISTS "${fullPath}")
          message(STATUS "DOES NOT EXIST: ${fullPath}")
        endif()
        #message(STATUS " Output Dir: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}")
        ADD_CUSTOM_TARGET(ZZ_${Z_LIBNAME}_DLL_${TYPE}-Copy ALL
                          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${fullPath}
                          ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}
                          COMMENT "  Copy: ${fullPath} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}"
                          )
        set_target_properties(ZZ_${Z_LIBNAME}_DLL_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/${Z_LIBNAME})

      endif()
    endif()
  endforeach()
endfunction()


#----------------------------------------------------------------
# Script starts here
#
if(MSVC_IDE)
    set(BUILD_TYPES Debug Release)
else()
    set(BUILD_TYPES "${CMAKE_BUILD_TYPE}")
    if("${BUILD_TYPES}" STREQUAL "")
        set(BUILD_TYPES "Debug")
    endif()
endif()

set(IFORT_COMPILER_ARCH_DIR "ia32")
if (${CMAKE_SIZEOF_VOID_P} EQUAL 8)
    set(IFORT_COMPILER_ARCH_DIR "intel64")
endif()

#-------------------------------------------------------------------------------
# This bit of code finds the "compiler" and "redist" directories based on the 
# current fortran compiler (which should be IFORT). This setup allows for 
# multiple versions of IFort to be used on the same system.
#
get_filename_component(IFORT_COMPILER_ROOT_DIR ${CMAKE_Fortran_COMPILER} DIRECTORY)
get_filename_component(IFORT_COMPILER_ROOT_DIR ${IFORT_COMPILER_ROOT_DIR} DIRECTORY)
get_filename_component(IFORT_COMPILER_ROOT_DIR ${IFORT_COMPILER_ROOT_DIR} DIRECTORY)

set(IFORT_COMPILER_RDIST_DIR "${IFORT_COMPILER_ROOT_DIR}/redist")
set(IFORT_COMPILER_ROOT_DIR "${IFORT_COMPILER_ROOT_DIR}/compiler")

if(CMAKE_FIND_DEBUG_MODE)
  message(STATUS "CMAKE_Fortran_COMPILER:  ${CMAKE_Fortran_COMPILER}")
  message(STATUS "IFORT_COMPILER_ROOT_DIR: ${IFORT_COMPILER_ROOT_DIR}")
  message(STATUS "IFORT_COMPILER_ARCH_DIR: ${IFORT_COMPILER_ARCH_DIR}")
  message(STATUS "IFORT_COMPILER_RDIST_DIR: ${IFORT_COMPILER_RDIST_DIR}")
endif()

set(IFORT_COMPILER_RDIST_LIBRARIES "")
set(IFORT_COMPILER_LIBRARIES "")

if(WIN32)
  AddIFortCopyInstallRules(LIBNAME ifcoremd
                          LIBPREFIX lib
                          LIBPATH ${IFORT_COMPILER_RDIST_DIR}/${IFORT_COMPILER_ARCH_DIR}/compiler
                          TYPES ${BUILD_TYPES})
  AddIFortCopyInstallRules(LIBNAME mmd
                          LIBPREFIX lib
                          LIBPATH ${IFORT_COMPILER_RDIST_DIR}/${IFORT_COMPILER_ARCH_DIR}/compiler
                          TYPES ${BUILD_TYPES})

  # These next libraries do not seem to have a debug version....
  set(BUILD_TYPES Release)
  AddIFortCopyInstallRules(LIBNAME ifportmd
                          LIBPREFIX lib
                          LIBPATH ${IFORT_COMPILER_RDIST_DIR}/${IFORT_COMPILER_ARCH_DIR}/compiler
                          TYPES ${BUILD_TYPES})
  AddIFortCopyInstallRules(LIBNAME iomp5md
                          LIBPREFIX lib
                          LIBPATH ${IFORT_COMPILER_RDIST_DIR}/${IFORT_COMPILER_ARCH_DIR}/compiler
                          TYPES ${BUILD_TYPES})
  AddIFortCopyInstallRules(LIBNAME svml_dispmd
                          LIBPREFIX ""
                          LIBPATH ${IFORT_COMPILER_RDIST_DIR}/${IFORT_COMPILER_ARCH_DIR}/compiler
                          TYPES ${BUILD_TYPES})
endif()

# If we are using IFort
set(FORTRAN_OPEN_MP_DEFS "")
if (Fortran_COMPILER_NAME MATCHES "ifort.*")
  if(WIN32)
    set(FORTRAN_OPEN_MP_DEFS "/Qopenmp /Qdiag-disable:11082 /Qip")
  else()
    set(FORTRAN_OPEN_MP_DEFS "-qopenmp -assume byterecl")
  endif()
endif()



