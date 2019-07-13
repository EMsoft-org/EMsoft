
# -------------------------------------------------------------
# This function adds the necessary cmake code to find the EbsdLib
# shared libraries and setup custom copy commands and/or install
# rules for Linux and Windows to use
function(AddEbsdLibCopyInstallRules)
  set(options )
  set(oneValueArgs LIBNAME LIBVAR)
  set(multiValueArgs TYPES)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(INTER_DIR ".")

 # message(STATUS "Z_LIBVAR: ${Z_LIBVAR}")
 # message(STATUS "Z_LIBNAME: ${Z_LIBNAME}")
 # message(STATUS "Z_TYPES: ${Z_TYPES}")
  
  set(Z_INSTALL_DIR "lib")
  if(WIN32)
    set(Z_INSTALL_DIR "bin")
  endif()
 # message(STATUS "Z_INSTALL_DIR: ${Z_INSTALL_DIR}")


  FOREACH(BTYPE ${Z_TYPES} )
    # message(STATUS "BTYPE: ${BTYPE}")
    STRING(TOUPPER ${BTYPE} TYPE)
    if(MSVC_IDE)
      set(INTER_DIR "${BTYPE}")
    endif()

    # Get the Actual Library Path and create Install and copy rules
    GET_TARGET_PROPERTY(LibPath ${Z_LIBNAME} IMPORTED_LOCATION_${TYPE})
    # message(STATUS "LibPath: ${LibPath}")
    if(NOT "${LibPath}" STREQUAL "LibPath-NOTFOUND")
     
      if(NOT TARGET ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy)
       # message(STATUS "Creating Install Rule for ${LibPath}")
        ADD_CUSTOM_TARGET(ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LibPath}
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                            # COMMENT "  Copy: ${LibPath} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                            )
        set_target_properties(ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/EbsdLib)
        install(FILES ${LibPath} DESTINATION "${Z_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy)
      endif()
    endif()

    # Now get the path that the library is in
    GET_FILENAME_COMPONENT(${Z_LIBVAR}_DIR ${LibPath} PATH)
    # message(STATUS "${Z_LIBVAR}_DIR: ${${Z_LIBVAR}_DIR}")

    # Now piece together a complete path for the symlink that Linux Needs to have
    if(WIN32)
      GET_TARGET_PROPERTY(${Z_LIBVAR}_${TYPE} ${Z_LIBNAME} IMPORTED_IMPLIB_${TYPE})
    else()
      GET_TARGET_PROPERTY(${Z_LIBVAR}_${TYPE} ${Z_LIBNAME} IMPORTED_SONAME_${TYPE})
    endif()

    # message(STATUS "${Z_LIBVAR}_${TYPE}: ${${Z_LIBVAR}_${TYPE}}")
    if(NOT "${${Z_LIBVAR}_${TYPE}}" STREQUAL "${Z_LIBVAR}_${TYPE}-NOTFOUND" AND NOT WIN32)
      set(SYMLINK_PATH "${${Z_LIBVAR}_DIR}/${${Z_LIBVAR}_${TYPE}}")
      # message(STATUS "Creating Install Rule for ${SYMLINK_PATH}")
      if(NOT TARGET ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy)
        ADD_CUSTOM_TARGET(ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SYMLINK_PATH}
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                            # COMMENT "  Copy: ${SYMLINK_PATH} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                            )
        set_target_properties(ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/EbsdLib)
        install(FILES ${SYMLINK_PATH} DESTINATION "${Z_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy)
      endif()
    endif()

  endforeach()
endfunction()


#------------------------------------------------------------------------------
# Find EbsdLib Headers/Libraries
# EbsdLib now comes with everything that is needed for CMake to load
# up the targets (Exported) that it needs. We just need to find where EbsdLib is installed.
#------------------------------------------------------------------------------
if("${EbsdLib_DIR}" STREQUAL "")
    set(EbsdLib_DIR  $ENV{EbsdLib_DIR})
endif()


# message(STATUS "EbsdLib_DIR: ${EbsdLib_DIR}")

find_package(EbsdLib)
if(NOT EbsdLib_FOUND)
  # message(FATAL_ERROR "EbsdLib was not found on your system. Please follow any instructions given to fix the problem")
endif()

if(EbsdLib_FOUND)
  # Add the library directory to the file that has all the search directories stored in it.
  get_property(EbsdLib_STATUS_PRINTED GLOBAL PROPERTY EbsdLib_STATUS_PRINTED)
  if(NOT EbsdLib_STATUS_PRINTED)
    # message(STATUS "EbsdLib Location: ${EbsdLib_DIR}")
    set_property(GLOBAL PROPERTY EbsdLib_STATUS_PRINTED TRUE)
    file(APPEND ${CMP_PLUGIN_SEARCHDIR_FILE} "${EbsdLib_LIB_DIRS};")
  endif()

  if(MSVC_IDE)
    set(BUILD_TYPES Debug Release)
  else()
    set(BUILD_TYPES "${CMAKE_BUILD_TYPE}")
    if("${BUILD_TYPES}" STREQUAL "")
        set(BUILD_TYPES "Debug")
    endif()
  endif()


  if(NOT APPLE)
    AddEbsdLibCopyInstallRules(LIBVAR EbsdLib_LIB
                        LIBNAME EbsdLib::EbsdLib
                        TYPES ${BUILD_TYPES})
  endif()

  # The next CMake variable is needed for Linux to properly generate a shell script
  # that will properly install the EbsdLib files.
  if(NOT APPLE AND NOT WIN32)
    STRING(TOUPPER "${CMAKE_BUILD_TYPE}" TYPE)
    get_target_property(EbsdLib_C_LIB_PATH  EbsdLib::hpdf IMPORTED_LOCATION_${TYPE})
    set(EbsdLib_COMPONENTS ${EbsdLib_C_LIB_PATH})
  endif()

ELSE(EbsdLib_FOUND)
    # message(FATAL_ERROR "Cannot build without EbsdLib.  Please set EbsdLib_INSTALL environment variable to point to your EbsdLib installation.")
ENDif(EbsdLib_FOUND)

