
# -------------------------------------------------------------
# This function adds the necessary cmake code to find the HDF5
# shared libraries and setup custom copy commands and/or install
# rules for Linux and Windows to use
function(AddHDF5CopyInstallRules)
  set(options )
  set(oneValueArgs LIBNAME LIBVAR)
  set(multiValueArgs TYPES)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  set(INTER_DIR "")

  if(0)
    message(STATUS "Z_LIBNAME: ${Z_LIBNAME}")
    message(STATUS "Z_LIBVAR: ${Z_LIBVAR}")
    message(STATUS "Z_TYPES: ${Z_TYPES}") 
  endif(0)
  
  set(h5LibName ${Z_LIBNAME})
  if (HDF5_VERSION_STRING VERSION_GREATER 1.8.15)
    if(${HDF5_BUILD_SHARED_LIBS})
      set(h5LibName hdf5::${Z_LIBNAME}-shared)
    elseif(APPLE)
      set(h5LibName hdf5::${Z_LIBNAME}-static)
    endif()
  endif()


  set(Z_INSTALL_DIR "lib")
  if(WIN32)
    set(Z_INSTALL_DIR "bin")
  endif()

  FOREACH(BTYPE ${Z_TYPES} )
    # message(STATUS "  BTYPE: ${BTYPE}")
    STRING(TOUPPER ${BTYPE} TYPE)
    if(MSVC_IDE)
      set(INTER_DIR "${BTYPE}/")
    endif()

    # Get the Actual Library Path and create Install and copy rules
    GET_TARGET_PROPERTY(LibPath ${h5LibName} IMPORTED_LOCATION_${TYPE})
    #message(STATUS "  LibPath: ${LibPath}")
    if(NOT "${LibPath}" STREQUAL "LibPath-NOTFOUND")
      if(NOT TARGET ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy)
        # message(STATUS "Creating Install And Copy Rule for ${LibPath}")
        install(FILES ${LibPath}
          DESTINATION "${Z_INSTALL_DIR}"
          CONFIGURATIONS ${BTYPE}
          COMPONENT Applications)

        if(NOT EXISTS "${LibPath}")
          message(STATUS "DOES NOT EXIST: ${LibPath}")
        endif()
        # message(STATUS "    ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}")
        ADD_CUSTOM_TARGET(ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy ALL
                          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LibPath}
                          ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}
                          COMMENT "  Copy: ${LibPath} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}"
                          )
        set_target_properties(ZZ_${Z_LIBVAR}_DLL_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/${Z_LIBVAR})

      endif()
    endif()

if(0)
    # Now get the path that the library is in
    GET_FILENAME_COMPONENT(${Z_LIBVAR}_DIR ${LibPath} PATH)
    #message(STATUS "  ${Z_LIBVAR}_DIR: ${${Z_LIBVAR}_DIR}")

    # Now piece together a complete path for the symlink that Linux Needs to have
    if(WIN32)
      GET_TARGET_PROPERTY(${Z_LIBVAR}_${TYPE} ${h5LibName} IMPORTED_IMPLIB_${TYPE})
    else()
      GET_TARGET_PROPERTY(${Z_LIBVAR}_${TYPE} ${h5LibName} IMPORTED_SONAME_${TYPE})
    endif()

    #message(STATUS "  ${Z_LIBVAR}_${TYPE}: ${${Z_LIBVAR}_${TYPE}}")
    if(NOT "${${Z_LIBVAR}_${TYPE}}" STREQUAL "${Z_LIBVAR}_${TYPE}-NOTFOUND" AND NOT WIN32)
      set(SYMLINK_PATH "${${Z_LIBVAR}_DIR}/${${Z_LIBVAR}_${TYPE}}")
      if(NOT TARGET ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy)
        #message(STATUS "    Creating Install and Copy Rule for ${SYMLINK_PATH}")
        install(FILES ${SYMLINK_PATH}
          DESTINATION "${Z_INSTALL_DIR}"
          CONFIGURATIONS ${BTYPE}
          COMPONENT Applications)

        ADD_CUSTOM_TARGET(ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy ALL
                            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SYMLINK_PATH}
                            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/
                            COMMENT "  Copy: ${SYMLINK_PATH} To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INTER_DIR}/"
                            )
        set_target_properties(ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES)
        set_target_properties(ZZ_${Z_LIBVAR}_SYMLINK_${TYPE}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${BTYPE}/${Z_LIBVAR})        
      endif()
    endif()
endif()

  endforeach()
endfunction()

#------------------------------------------------------------------------------
# Find HDF5 Headers/Libraries
# HDF5 now comes with everything that is needed for CMake to load
# up the targets (Exported) that it needs. We just need to find where HDF5 is installed.
#------------------------------------------------------------------------------
if( "${HDF5_DIR}" STREQUAL "")
  message(FATAL_ERROR "The HDF5_DIR variable was not set. In order to find HDF5 you need to set or\
                      pass in the -DHDF5_DIR=  setting")
endif()

#message(STATUS "LIB_TYPE: ${LIB_TYPE}")
string(TOLOWER ${LIB_TYPE} SEARCH_TYPE)
find_package(HDF5 NAMES hdf5 COMPONENTS C HL Fortran Fortran_HL)

if(HDF5_FOUND)
  
  GET_FILENAME_COMPONENT (HDF5_ROOT_DIR "${HDF5_INCLUDE_DIR}" PATH)

  set(HDF5_LIBRARY_DIRS ${HDF5_ROOT_DIR}/lib)
  if(NOT "${CMP_PLUGIN_SEARCHDIR_FILE}" STREQUAL "")
    file(APPEND ${CMP_PLUGIN_SEARCHDIR_FILE} "${HDF5_LIBRARY_DIRS};")
  endif()
  # Add the library directory to the file that has all the search directories stored in it.

  include_directories(${HDF5_INCLUDE_DIRS}) #HDF5 1.8.15 and below
  include_directories(${HDF5_INCLUDE_DIR}) #HDF5 1.8.16 and above

  #include_directories(${${HDF5_PACKAGE_NAME}_INCLUDE_DIR_FORTRAN})
  if (HDF5_VERSION_STRING VERSION_GREATER 1.8.15)
    if(${HDF5_BUILD_FORTRAN})
      if(${HDF5_BUILD_SHARED_LIBS})
        set(HDF5_INCLUDE_DIR_FORTRAN "${HDF5_ROOT_DIR}/include/shared")
      else()
        set(HDF5_INCLUDE_DIR_FORTRAN "${HDF5_ROOT_DIR}/include/static")
      endif()
    endif()
  endif()
  message(STATUS "HDF5 [${HDF5_VERSION_STRING}] Location: ${HDF5_DIR}")
  # message(STATUS "HDF5 Version: ")
  # message(STATUS "HDF5_BUILD_FORTRAN: ${HDF5_BUILD_FORTRAN}")
  # message(STATUS "HDF5_BUILD_SHARED_LIBS : ${HDF5_BUILD_SHARED_LIBS}")
  # message(STATUS "HDF5_INCLUDE_DIR_FORTRAN: ${HDF5_INCLUDE_DIR_FORTRAN}")

  if(MSVC_IDE)
    set(BUILD_TYPES Debug Release)
  else()
    set(BUILD_TYPES "${CMAKE_BUILD_TYPE}")
    if("${BUILD_TYPES}" STREQUAL "")
        set(BUILD_TYPES "Debug")
    endif()
  endif()


  if(WIN32)
    #hdf5 hdf5_f90cstub hdf5_fortran hdf5_hl_fortran
    AddHDF5CopyInstallRules(LIBVAR HDF5_LIB
                        LIBNAME hdf5
                        TYPES ${BUILD_TYPES})
    AddHDF5CopyInstallRules(LIBVAR HDF5_HL_LIB
                        LIBNAME hdf5_hl
                        TYPES ${BUILD_TYPES})
    AddHDF5CopyInstallRules(LIBVAR HDF5_CPP_LIB
                        LIBNAME hdf5_cpp
                        TYPES ${BUILD_TYPES})
    AddHDF5CopyInstallRules(LIBVAR HDF5_F90CSTUB_LIB
                        LIBNAME hdf5_f90cstub
                        TYPES ${BUILD_TYPES})
    AddHDF5CopyInstallRules(LIBVAR HDF5_FORTRAN_LIB
                        LIBNAME hdf5_fortran
                        TYPES ${BUILD_TYPES})
    AddHDF5CopyInstallRules(LIBVAR HDF5_HL_FORTRAN_LIB
                        LIBNAME hdf5_hl_fortran
                        TYPES ${BUILD_TYPES})
    AddHDF5CopyInstallRules(LIBVAR HDF5_HL_F90CSTUB_LIB
                        LIBNAME hdf5_hl_f90cstub
                        TYPES ${BUILD_TYPES})
  endif()

  set(HDF5_COMPONENTS hdf5 hdf5_hl hdf5_cpp hdf5_f90cstub hdf5_fortran hdf5_hl_fortran hdf5_hl_f90cstub)


ELSE(HDF5_FOUND)
    MESSAGE(FATAL_ERROR "Cannot build without HDF5.  Please set HDF5_INSTALL environment variable to point to your HDF5 installation.")
ENDif(HDF5_FOUND)

