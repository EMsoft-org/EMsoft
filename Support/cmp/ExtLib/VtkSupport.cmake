# -------------------------------------------------------------
# This function adds the necessary cmake code to find the Vtk
# shared libraries and setup custom copy commands and/or install
# rules for Linux and Windows to use
function(AddVtkCopyInstallRules)
  set(options )
  set(oneValueArgs )
  set(multiValueArgs LIBS)
  cmake_parse_arguments(vtk "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(INTER_DIR ".")


  if(MSVC_IDE)
    set(vtk_TYPES Debug Release)
  else()
    set(vtk_TYPES "${CMAKE_BUILD_TYPE}")
    if("${vtk_TYPES}" STREQUAL "")
        set(vtk_TYPES "Debug")
    endif()
  endif()


  # message(STATUS "vtk_LIBNAME: ${vtk_LIBNAME}")
  # message(STATUS "vtk_LIBVAR: ${vtk_LIBVAR}")
  # message(STATUS "vtk_TYPES: ${vtk_TYPES}")

  set(vtk_INSTALL_DIR "lib")
  if(WIN32)
    set(vtk_INSTALL_DIR ".")
  endif()


  set(STACK "")
  list(APPEND STACK ${vtk_LIBS})
  # message(STATUS "STACK: ${STACK}")
  # While the depth-first search stack is not empty
  list(LENGTH STACK STACK_LENGTH)
  # message(STATUS "STACK_LENGTH: ${STACK_LENGTH}")

  while(STACK_LENGTH GREATER 0)

    # This pair of commands "pops" the last value off the
    # stack and sets the STACK_LENGTH variable
    list(GET STACK 0 vtk_LIBNAME)
    list(REMOVE_AT STACK 0)
    list(LENGTH STACK STACK_LENGTH)

    # See if we have found a Qt5 or System library. All Vtk libs start with "vtk"
    string(FIND ${vtk_LIBNAME} "Qt5::" IsQt5Lib)
    string(REGEX MATCH "^vtk" IsVtkLib ${vtk_LIBNAME})

    # If we have not seen this library before then find its dependencies
    if(NOT FOUND_${vtk_LIBNAME})
      set(FOUND_${vtk_LIBNAME} TRUE)
      if(${IsQt5Lib} EQUAL -1 AND "${IsVtkLib}" STREQUAL "vtk")
        # message(STATUS "    ${vtk_LIBNAME}: ${FOUND_${vtk_LIBNAME}}  IsVtkLib: ${IsVtkLib}")
        set(vtk_LIBVAR ${vtk_LIBNAME})
        foreach(BTYPE ${vtk_TYPES} )
          # message(STATUS "  BTYPE: ${BTYPE}")
          string(TOUPPER ${BTYPE} UpperBType)
          if(MSVC_IDE)
            set(INTER_DIR "${BTYPE}")
          endif()

          # Find the current library's dependent Vtk libraries
          get_target_property(vtkLibDeps ${vtk_LIBNAME} IMPORTED_LINK_DEPENDENT_LIBRARIES_${UpperBType})
          if(NOT "${vtkLibDeps}" STREQUAL "vtkLibDeps-NOTFOUND" )
            list(APPEND STACK ${vtkLibDeps})
          else()
           # message(STATUS "---->${vtk_LIBNAME} IMPORTED_LINK_DEPENDENT_LIBRARIES_${UpperBType} NOT FOUND")
          endif()

          get_target_property(vtkLibDeps ${vtk_LIBNAME} INTERFACE_LINK_LIBRARIES)
          if(NOT "${vtkLibDeps}" STREQUAL "vtkLibDeps-NOTFOUND" )
            list(APPEND STACK ${vtkLibDeps})
          else()
            # message(STATUS "---->${vtk_LIBNAME} INTERFACE_LINK_LIBRARIES NOT FOUND")
          endif()

          # Get the Actual Library Path and create Install and copy rules
          get_target_property(DllLibPath ${vtk_LIBNAME} IMPORTED_LOCATION_${UpperBType})
          # message(STATUS "  DllLibPath: ${DllLibPath}")
          if(NOT "${DllLibPath}" STREQUAL "LibPath-NOTFOUND")
            # message(STATUS "  Creating Install Rule for ${DllLibPath}")
            if(NOT TARGET ZZ_${vtk_LIBVAR}_DLL_${UpperBType}-Copy)
              add_custom_target(ZZ_${vtk_LIBVAR}_DLL_${UpperBType}-Copy ALL
                                  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${DllLibPath}
                                  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                                  # COMMENT "  Copy: ${DllLibPath} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                                  )
              set_target_properties(ZZ_${vtk_LIBVAR}_DLL_${UpperBType}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/Vtk)
              install(FILES ${DllLibPath} DESTINATION "${vtk_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
              get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
              set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${vtk_LIBVAR}_DLL_${UpperBType}-Copy)
            endif()
          endif()

          # Now get the path that the library is in
          get_filename_component(${vtk_LIBVAR}_DIR ${DllLibPath} PATH)
          # message(STATUS " ${vtk_LIBVAR}_DIR: ${${vtk_LIBVAR}_DIR}")

          # Now piece together a complete path for the symlink that Linux Needs to have
          if(WIN32)
            get_target_property(${vtk_LIBVAR}_${UpperBType} ${vtk_LIBNAME} IMPORTED_IMPLIB_${UpperBType})
          else()
            get_target_property(${vtk_LIBVAR}_${UpperBType} ${vtk_LIBNAME} IMPORTED_SONAME_${UpperBType})
          endif()

          #----------------------------------------------------------------------
          # This section for Linux only
          # message(STATUS "  ${vtk_LIBVAR}_${UpperBType}: ${${vtk_LIBVAR}_${UpperBType}}")
          if(NOT "${${vtk_LIBVAR}_${UpperBType}}" STREQUAL "${vtk_LIBVAR}_${UpperBType}-NOTFOUND" AND NOT WIN32)
            set(SYMLINK_PATH "${${vtk_LIBVAR}_DIR}/${${vtk_LIBVAR}_${UpperBType}}")
            # message(STATUS "  Creating Install Rule for ${SYMLINK_PATH}")
            if(NOT TARGET ZZ_${vtk_LIBVAR}_SYMLINK_${UpperBType}-Copy)
              add_custom_target(ZZ_${vtk_LIBVAR}_SYMLINK_${UpperBType}-Copy ALL
                                  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SYMLINK_PATH}
                                  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                                  # COMMENT "  Copy: ${SYMLINK_PATH} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                                  )
              set_target_properties(ZZ_${vtk_LIBVAR}_SYMLINK_${UpperBType}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/Vtk)
              install(FILES ${SYMLINK_PATH} DESTINATION "${vtk_INSTALL_DIR}" CONFIGURATIONS ${BTYPE} COMPONENT Applications)
              get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
              set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${vtk_LIBVAR}_SYMLINK_${UpperBType}-Copy) 
            endif()
          endif()
          # End Linux Only Section
          #------------------------------------------------------------------------

        endforeach()
      endif()
    else()
      # message(STATUS "----> Already Found ${vtk_LIBNAME}")
    endif()

    # Remove duplicates and set the stack_length variable (VERY IMPORTANT)
    list(REMOVE_DUPLICATES STACK)
    list(LENGTH STACK STACK_LENGTH)

  endwhile()

endfunction()

# --------------------------------------------------------------------
# 
function(CMP_FindVtkComponents)
  set(options)
  set(oneValueArgs QT5_REQUIRED)
  set(multiValueArgs COMPONENTS)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  find_package(VTK COMPONENTS ${Z_COMPONENTS})
  if(NOT VTK_FOUND)
    message(FATAL_ERROR "Vtk is required for this build. One of the projects asked to find Vtk and Vtk was not located.")
  endif()

  include(${VTK_USE_FILE})

  if(Z_QT5_REQUIRED)
    if("${VTK_QT_VERSION}" STREQUAL "")
      message(FATAL_ERROR "VTK was not built with Qt")
    endif()
  endif()

  # --------------------------------------------------------------------
  # If we are NOT on Apple platform then create the copy and install rules
  # for all of the dependant Vtk libraries.
  if(NOT APPLE)
    AddVtkCopyInstallRules(LIBS ${Z_COMPONENTS})
  endif()

endfunction()

# # --------------------------------------------------------------------
# # Look for Vtk 7.0 as we need it for the plugin GUI to be generated
# # These are the required component libraries
# # The user of this module needs to set the VtkComponents
# # variable like the example below which will include all the needed
# # vtk modules.
# set(SIMPL_Base_VtkComponents
#    vtkGUISupportQt
#    vtkRenderingCore
#    vtkRenderingFreeType
#    vtkRenderingVolumeOpenGL2
#    vtkRenderingAnnotation
#    vtkInteractionWidgets
#    vtkInteractionStyle
#    vtkIOLegacy
#    vtkIOImage
#  )
# if(NOT DEFINED SIMPL_Base_VtkComponents_Configured)
#   set(SIMPL_Base_VtkComponents_Configured "ON" CACHE STRING "")
#   list(APPEND CMP_VtkComponents ${SIMPL_Base_VtkComponents})
# endif()
# #--- Append on what another library or plugin wants
# list(APPEND CMP_VtkComponents ${VtkComponents})
# #--- Remove all the duplicate components
# message(STATUS "CMP_VtkComponents: ${CMP_VtkComponents}")
# list(REMOVE_DUPLICATES CMP_VtkComponents)




