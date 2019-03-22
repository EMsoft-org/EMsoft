# -------------------------------------------------------------
# This function adds the necessary cmake code to find the qxorm
# shared libraries and setup custom copy commands and/or install
# rules for Linux and Windows to use
function(AddQxOrmCopyInstallRules)
  set(options )
  set(oneValueArgs )
  set(multiValueArgs )
  cmake_parse_arguments(qxorm "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(INTER_DIR ".")


  if(MSVC_IDE)
    set(qxorm_TYPES Debug Release)
  else()
    set(qxorm_TYPES "${CMAKE_BUILD_TYPE}")
    if("${qxorm_TYPES}" STREQUAL "")
        set(qxorm_TYPES "Debug")
    endif()
  endif()

  if(WIN32)
    set(qxorm_LIB_TYPE "dll")
  else()
    set(qxorm_LIB_TYPE "so")
  endif()

  set(qxorm_INSTALL_DIR "lib")
  if(WIN32)
    set(qxorm_INSTALL_DIR ".")
  endif()

  set(qxorm_LIB_DIR "${QXORM_DIR}/lib")
  set(qxorm_LIBVAR "QxOrm")
  foreach(BTYPE ${qxorm_TYPES} )
    # message(STATUS "  BTYPE: ${BTYPE}")
    string(TOUPPER ${BTYPE} UpperBType)
    if(MSVC_IDE)
      set(INTER_DIR "${BTYPE}")
    endif()

    # Set DLL path
    if(${UpperBType} STREQUAL "DEBUG")
      set(qxorm_LIBS "QxOrmd")
    else()
      set(qxorm_LIBS "QxOrm")
    endif()
    set(DllLibPath "${qxorm_LIB_DIR}/${qxorm_LIBS}.${qxorm_LIB_TYPE}")

    # Get the Actual Library Path and create Install and copy rules
    # get_target_property(DllLibPath ${qxorm_LIBNAME} IMPORTED_LOCATION_${UpperBType})
    # message(STATUS "  DllLibPath: ${DllLibPath}")
    if(NOT "${DllLibPath}" STREQUAL "LibPath-NOTFOUND")
      # message(STATUS "  Creating Install Rule for ${DllLibPath}")
      if(NOT TARGET ZZ_${qxorm_LIBVAR}_DLL_${UpperBType}-Copy)
        add_custom_target(ZZ_${qxorm_LIBVAR}_DLL_${UpperBType}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${DllLibPath}
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                            # COMMENT "  Copy: ${DllLibPath} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                            )
        set_target_properties(ZZ_${qxorm_LIBVAR}_DLL_${UpperBType}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/QxOrm)
        install(FILES ${DllLibPath} DESTINATION "${qxorm_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${qxorm_LIBVAR}_DLL_${UpperBType}-Copy)
      endif()
    endif()

    #----------------------------------------------------------------------
    # This section for Linux only
    # message(STATUS "  ${qxorm_LIBVAR}_${UpperBType}: ${${qxorm_LIBVAR}_${UpperBType}}")
    if(NOT "${${qxorm_LIBVAR}_${UpperBType}}" STREQUAL "${qxorm_LIBVAR}_${UpperBType}-NOTFOUND" AND NOT WIN32)
      set(SYMLINK_PATH "${${qxorm_LIBVAR}_DIR}/${${qxorm_LIBVAR}_${UpperBType}}")
      # message(STATUS "  Creating Install Rule for ${SYMLINK_PATH}")
      if(NOT TARGET ZZ_${qxorm_LIBVAR}_SYMLINK_${UpperBType}-Copy)
        add_custom_target(ZZ_${qxorm_LIBVAR}_SYMLINK_${UpperBType}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SYMLINK_PATH}
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                            # COMMENT "  Copy: ${SYMLINK_PATH} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                            )
        set_target_properties(ZZ_${qxorm_LIBVAR}_SYMLINK_${UpperBType}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/qxorm)
        install(FILES ${SYMLINK_PATH} DESTINATION "${qxorm_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${qxorm_LIBVAR}_SYMLINK_${UpperBType}-Copy) 
      endif()
    endif()
    # End Linux Only Section
    #------------------------------------------------------------------------

  endforeach()

endfunction()

# --------------------------------------------------------------------
# 
function(CMP_FindQxOrm)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  # --------------------------------------------------------------------
  # If we are NOT on Apple platform then create the copy and install rules
  # for all of the dependant qxorm libraries.
  if(NOT APPLE)
    AddQxOrmCopyInstallRules()
  endif()

endfunction()

