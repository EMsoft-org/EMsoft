# -------------------------------------------------------------
# This function adds the necessary cmake code to find the PCL
# shared libraries and setup custom copy commands and/or install
# rules for Linux and Windows to use
function(AddPCLCopyInstallRules)
  set(options )
  set(oneValueArgs )
  set(multiValueArgs LIBS TYPES)
  cmake_parse_arguments(pcl "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(INTER_DIR ".")

  #message(STATUS "CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")
  if(MSVC_IDE)
    set(build_types Debug Release)
  else()
    set(build_types "${CMAKE_BUILD_TYPE}")
    if("${build_types}" STREQUAL "")
        set(build_types "Debug")
    endif()
  endif()

  set(pcl_INSTALL_DIR "lib")

  if(WIN32)
    set(pcl_INSTALL_DIR ".")
  endif()


  set(STACK "")
  list(APPEND STACK ${pcl_LIBS})
  # While the depth-first search stack is not empty
  list(LENGTH STACK STACK_LENGTH)

  while(STACK_LENGTH GREATER 0)

    # This pair of commands "pops" the last value off the
    # stack and sets the STACK_LENGTH variable
    list(GET STACK 0 pcl_LIBNAME)
    list(REMOVE_AT STACK 0)
    list(LENGTH STACK STACK_LENGTH)

    # If we have not seen this library before then find its dependencies
    if(NOT FOUND_${pcl_LIBNAME})
      set(FOUND_${pcl_LIBNAME} TRUE)
      # message(STATUS "    ${pcl_LIBNAME}: ${FOUND_${pcl_LIBNAME}}  IsPCLLib: ${IsPCLLib}")
      set(pcl_LIBVAR ${pcl_LIBNAME})
      foreach(BTYPE ${build_types} )

        string(TOUPPER ${BTYPE} UpperBType)
        if(MSVC_IDE)
          set(INTER_DIR "${BTYPE}")
        endif()

        # Find the current library's dependent PCL libraries
        list(APPEND STACK ${pcl_${pcl_LIBVAR}_int_dep})
        STRING(TOUPPER ${pcl_LIBVAR} PCOMP)

        set(VarSuffix "")
        if("${BTYPE}" STREQUAL "Debug")
          set(VarSuffix "_${UpperBType}")
        endif()
        # Get the Actual Library Path and create Install and copy rules
        GET_FILENAME_COMPONENT(PCL_COMP_NAME "${PCL_${PCOMP}_LIBRARY${VarSuffix}}" NAME_WE)
        GET_FILENAME_COMPONENT(PCL_LIB_DIR "${PCL_${PCOMP}_LIBRARY${VarSuffix}}" DIRECTORY)
        GET_FILENAME_COMPONENT(PCL_LIB_DIR "${PCL_LIB_DIR}" DIRECTORY)
        set(pcl_DLL_LIB_PATH ${PCL_LIB_DIR}/bin/${PCL_COMP_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX})
        # message(STATUS "pcl_DLL_LIB_PATH: ${pcl_DLL_LIB_PATH}")
        if(EXISTS "${pcl_DLL_LIB_PATH}")
          # message(STATUS "  Creating Install Rule for ${pcl_DLL_LIB_PATH}")
          if(NOT TARGET ZZ_${pcl_LIBVAR}_DLL_${UpperBType}-Copy)
            add_custom_target(ZZ_${pcl_LIBVAR}_DLL_${UpperBType}-Copy ALL
                                COMMAND ${CMAKE_COMMAND} -E copy_if_different ${pcl_DLL_LIB_PATH}
                                ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                                # COMMENT "  Copy: ${pcl_DLL_LIB_PATH} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                                )
            set_target_properties(ZZ_${pcl_LIBVAR}_DLL_${UpperBType}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/PCL)
            install(FILES ${pcl_DLL_LIB_PATH} DESTINATION "${pcl_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
            get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
            set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${pcl_LIBVAR}_DLL_${UpperBType}-Copy)
          endif()
        endif()

        # Now get the path that the library is in
        # get_filename_component(${pcl_LIBVAR}_DIR ${pcl_DLL_LIB_PATH} PATH)
        # message(STATUS " ${pcl_LIBVAR}_DIR: ${${pcl_LIBVAR}_DIR}")

        # Now piece together a complete path for the symlink that Linux Needs to have
        if(WIN32)
          #get_target_property(${pcl_LIBVAR}_${UpperBType} ${pcl_LIBNAME} IMPORTED_IMPLIB_${UpperBType})
        else()
          #get_target_property(${pcl_LIBVAR}_${UpperBType} ${pcl_LIBNAME} IMPORTED_SONAME_${UpperBType})
        endif()

        #----------------------------------------------------------------------
        # This section for Linux only
        #message(STATUS "  ${pcl_LIBVAR}_${UpperBType}: ${${pcl_LIBVAR}_${UpperBType}}")
        if(NOT "${${pcl_LIBVAR}_${UpperBType}}" STREQUAL "${pcl_LIBVAR}_${UpperBType}-NOTFOUND" AND NOT WIN32)
          set(SYMLINK_PATH "${${pcl_LIBVAR}_DIR}/${${pcl_LIBVAR}_${UpperBType}}")
          #message(STATUS "  Creating Install Rule for ${SYMLINK_PATH}")
          if(NOT TARGET ZZ_${pcl_LIBVAR}_SYMLINK_${UpperBType}-Copy)
            add_custom_target(ZZ_${pcl_LIBVAR}_SYMLINK_${UpperBType}-Copy ALL
                                COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SYMLINK_PATH}
                                ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                                # COMMENT "  Copy: ${SYMLINK_PATH} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                                )
            set_target_properties(ZZ_${pcl_LIBVAR}_SYMLINK_${UpperBType}-Copy PROPERTIES FOLDER ZZ_PCL_COPY_FILES/${BTYPE}/PCL)
            install(FILES ${SYMLINK_PATH} DESTINATION "${pcl_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
            get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
            set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${pcl_LIBVAR}_SYMLINK_${UpperBType}-Copy)
          endif()
        endif()
        # End Linux Only Section
        #------------------------------------------------------------------------

      endforeach()
  
    else()
      # message(STATUS "----> Already Found ${pcl_LIBNAME}")
    endif()

    # Remove duplicates and set the stack_length variable (VERY IMPORTANT)
    list(REMOVE_DUPLICATES STACK)
    list(LENGTH STACK STACK_LENGTH)
  endwhile()

endfunction()

# --------------------------------------------------------------------
# Look for PCL 7.0 as we need it for the plugin GUI to be generated
# These are the required PCL component libraries
# 
# Use the PCL_COMPONENTS variable to set up the boost libraries that your project needs to copy
# For example:
#  set(PCL_COMPONENTS search filters features surface keypoints registration)
# 
# Bring in PCL
find_package(PCL)
if (NOT PCL_FOUND)
  message(FATAL_ERROR "Unable to locate PCL")
endif()

# --------------------------------------------------------------------
# If we are NOT on Apple platform then create the copy and install rules
# for all of the dependant PCL libraries.
if(NOT APPLE)
  AddPCLCopyInstallRules(LIBS ${PCL_COMPONENTS})
endif()


