
function(AddBoostCopyInstallRules)
  set(options )
  set(oneValueArgs LIBNAME LIBVAR)
  set(multiValueArgs TYPES)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(INTER_DIR ".")

  #message(STATUS "Z_LIBNAME: ${Z_LIBNAME}")
  #message(STATUS "Z_LIBVAR: ${Z_LIBVAR}")
  #message(STATUS "Z_TYPES: ${Z_TYPES}")

  set(Z_INSTALL_DIR "lib")
  if(WIN32)
    set(Z_INSTALL_DIR ".")
  endif()

  FOREACH(BTYPE ${Z_TYPES} )
    #message(STATUS "BTYPE: ${BTYPE}")
    STRING(TOUPPER ${BTYPE} TYPE)
    if(MSVC_IDE)
      set(INTER_DIR "${BTYPE}")
    endif()

    # Get the Actual Library Path and create Install and copy rules
    GET_TARGET_PROPERTY(LibPath ${Z_LIBNAME} IMPORTED_LOCATION_${TYPE})
    GET_FILENAME_COMPONENT(Boost_COMP_NAME "${LibPath}" NAME_WE)
    GET_FILENAME_COMPONENT(Boost_LIB_DIR "${LibPath}" DIRECTORY)
    set(LibPath ${Boost_LIB_DIR}/${Boost_COMP_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX})

    # message(STATUS "LibPath: ${LibPath}")
    if(NOT "${LibPath}" STREQUAL "LibPath-NOTFOUND")
      # message(STATUS "Creating Install Rule for ${LibPath}")
      if(NOT TARGET ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy)
        ADD_CUSTOM_TARGET(ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LibPath}
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                            # COMMENT "  Copy: ${LibPath} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                            )
        set_target_properties(ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/Boost)
        install(FILES ${LibPath} DESTINATION "${Z_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
      endif()
    endif()




    # This next section needs to be completed for Linux
    #message(STATUS "${Z_LIBVAR}_${TYPE}: ${${Z_LIBVAR}_${TYPE}}")
    if(NOT "${${Z_LIBVAR}_${TYPE}}" STREQUAL "${Z_LIBVAR}_${TYPE}-NOTFOUND" AND NOT WIN32)
      message(FATAL_ERROR "Install rules not complete for Boost and Linux")
      set(SYMLINK_PATH "${${Z_LIBVAR}_DIR}/${${Z_LIBVAR}_${TYPE}}")
      #message(STATUS "Creating Install Rule for ${SYMLINK_PATH}")
      if(NOT TARGET ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy)
        ADD_CUSTOM_TARGET(ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SYMLINK_PATH}
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                            # COMMENT "  Copy: ${SYMLINK_PATH} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                            )
        set_target_properties(ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/Boost)
        install(FILES ${SYMLINK_PATH} DESTINATION "${Z_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
      endif()
    endif()

  endforeach()
endfunction()

# Use the BOOST_COMPONENTS variable to set up the boost libraries that your project needs to copy
# For example:
#  set(BOOST_COMPONENTS system filesystem thread date_time iostreams serialization chrono)
if(MSVC_IDE)
  set(BUILD_TYPES Debug Release)
else()
  set(BUILD_TYPES "${CMAKE_BUILD_TYPE}")
  if("${BUILD_TYPES}" STREQUAL "")
      set(BUILD_TYPES "Debug")
  endif()
endif()


if(NOT APPLE)
  foreach(COMP ${BOOST_COMPONENTS})
      AddBoostCopyInstallRules(LIBVAR ${COMP}
                          LIBNAME Boost::${COMP}
                          TYPES ${BUILD_TYPES})
  endforeach()
endif()
