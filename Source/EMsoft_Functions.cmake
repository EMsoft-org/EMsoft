
# #---------------------------------------------------------------------
# # Set some variables to shorten up the call to the function below
# set(APP_DIR "${EMsoft_SOURCE_DIR}/src_programs")
# set(TMPLT_DIR "${EMsoft_SOURCE_DIR}/NamelistTemplates")


#set(EXE_LINK_LIBRARIES EMsoftLib EMsoftHDFLib)


include_directories(${EMsoft_BINARY_DIR}/EMsoftLib)
include_directories(${EMsoftLib_BINARY_DIR})

#---------------------------------------------------------------------
# This function creates an executable that is to be compiled. The valid
# arguments are:
#  TARGET: the name of the executable
#  SOURCES: List of Fortran sources to be compiled
#  TEMPLATE The template file for the program
#  SOLUTION_FOLDER The Visual Studio or Xcode solution folder to include the program.
#  INSTALL_PROGRAM Set this to "TRUE" to have the program installed or included in the package
#  LINK_LIBRARIES: The list of Libraries the TARGET needs to be linked to
function(Add_EMsoft_Executable)
  set(options )
  set(oneValueArgs TARGET TEMPLATE SOLUTION_FOLDER INSTALL_PROGRAM)
  set(multiValueArgs SOURCES LINK_LIBRARIES)
  cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  # option(EMsoft_BUILD_${Z_TARGET} "Build ${Z_TARGET}" ON)
  # mark_as_advanced(EMsoft_BUILD_${Z_TARGET})

  include_directories(${CLFORTRAN_INCLUDE_DIR})
  include_directories(${HDF5_INCLUDE_DIR})

  #if(EMsoft_BUILD_${Z_TARGET})  
    set(install_dir "bin")
    set(lib_install_dir "lib")
    if("${Z_INSTALL_PROGRAM}" STREQUAL "")
      set(install_dir "")
      set(lib_install_dir "")
    elseif(WIN32)
      set(install_dir "bin")
      set(lib_install_dir "")
    elseif(APPLE)
      set(install_dir "bin")
      set(lib_install_dir "bin")
    endif()


    BuildToolBundle(TARGET ${Z_TARGET}
                  DEBUG_EXTENSION ${EXE_DEBUG_EXTENSION}
                  VERSION_MAJOR ${EMsoft_VER_MAJOR}
                  VERSION_MINOR ${EMsoft_VER_MINOR}
                  VERSION_PATCH ${EMsoft_VER_PATCH}
                  BINARY_DIR ${EMsoft_BINARY_DIR}/Applications/${Z_TARGET}
                  COMPONENT Applications
                  INSTALL_DEST "${install_dir}"
                  SOLUTION_FOLDER ${Z_SOLUTION_FOLDER}
                  SOURCES ${Z_SOURCES}
                  LINK_LIBRARIES ${Z_LINK_LIBRARIES}
    )
    if( NOT ${Z_SOLUTION_FOLDER} STREQUAL "")
      SET_TARGET_PROPERTIES(${Z_TARGET} PROPERTIES FOLDER ${Z_SOLUTION_FOLDER})
    endif()

  #endif()

  if(NOT "${Z_TEMPLATE}" STREQUAL "" AND NOT EXISTS ${Z_TEMPLATE})
    message(STATUS "Missing Template File for Executable ${Z_TARGET}")
    message(STATUS "  ${Z_TEMPLATE}")
  else()
    install(FILES ${Z_TEMPLATE}
      DESTINATION "NamelistTemplates"
      COMPONENT Applications
    )
  endif()

endfunction()

# --------------------------------------------------------------------------
# Converts file paths to use the '/' character so they are compatible with
# C/C++ language. The use of the "\" character would make the compiler think
# the following character would be escaped.
#-- Convert all '\' to '\\' so that they are properly escaped in the header file
macro(ConvertPathToHeaderCompatible INPUT)
    if(WIN32)
      STRING(REPLACE "\\" "\\\\" ${INPUT} ${${INPUT}} )
      STRING(REPLACE "/" "\\\\" ${INPUT} ${${INPUT}}  )
    endif()
endmacro()

#---------------------------------------------------------------------
# This function will add a new EMsoft unit test
function(AddEMsoftUnitTest)
    set(options)
    set(oneValueArgs TARGET SOLUTION_FOLDER TEST_NAME)
    set(multiValueArgs SOURCES LINK_LIBRARIES)
    cmake_parse_arguments(Z "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    set(TEST_NAME ${Z_TEST_NAME})
    set(TEST_SOURCE_FILE "${EMsoft_BINARY_DIR}/Source/Test/${Z_TEST_NAME}Test.cpp")
    configure_file("${EMsoft_SOURCE_DIR}/Source/Test/UnitTestMain.cpp.in"
                    "${TEST_SOURCE_FILE}" @ONLY)

    set(TEST_SOURCS
      "${Z_SOURCES}"
      "${TEST_SOURCE_FILE}"
      )

    add_executable( ${Z_TARGET} ${TEST_SOURCS})
    target_link_libraries( ${Z_TARGET} ${Z_LINK_LIBRARIES})
    if("${Z_SOLUTION_FOLDER}" STREQUAL "")
        set(Z_SOLUTION_FOLDER "Test")
    endif()
    set_target_properties( ${Z_TARGET} PROPERTIES FOLDER ${Z_SOLUTION_FOLDER})

    if(WIN32)
      set_target_properties(${Z_TARGET} PROPERTIES 
          LINK_FLAGS_DEBUG "/NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:msvcmrt.lib /NODEFAULTLIB:msvcurt.lib /NODEFAULTLIB:msvcrtd.lib"
          CMAKE_CXX_FLAGS_DEBUG "/D_DEBUG /MDd /Zi /Ob0 /Od /RTC1 /MTd"
          )
    endif()
    add_test(${Z_TARGET} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${Z_TARGET})

endfunction()

