#--////////////////////////////////////////////////////////////////////////////
#--
#--  Copyright (c) 2015, BlueQuartz Software
#--  All rights reserved.
#--  BSD License: http://www.opensource.org/licenses/bsd-license.html
#--
#-- This code was partly written under US Air Force Contract FA8650-07-D-5800
#--
#--////////////////////////////////////////////////////////////////////////////

# project(H5SupportTest)
set(PLUGIN_NAME "H5Support")
set(${PLUGIN_NAME}Test_SOURCE_DIR ${${PLUGIN_NAME}_SOURCE_DIR}/Test)
set(${PLUGIN_NAME}Test_BINARY_DIR ${${PLUGIN_NAME}_BINARY_DIR}/Test)

include(${CMP_SOURCE_DIR}/cmpCMakeMacros.cmake)




set(TEST_TEMP_DIR ${${PLUGIN_NAME}Test_BINARY_DIR}/Test/Temp)
# Make sure the directory is created during CMake time
file(MAKE_DIRECTORY ${TEST_TEMP_DIR})

configure_file(${${PLUGIN_NAME}_SOURCE_DIR}/Test/TestFileLocations.h.in
          ${${PLUGIN_NAME}_BINARY_DIR}/Test/${PLUGIN_NAME}TestFileLocations.h @ONLY IMMEDIATE)

configure_file(${SIMPLProj_SOURCE_DIR}/Resources/UnitTestSupport.hpp
          ${${PLUGIN_NAME}_BINARY_DIR}/Test/UnitTestSupport.hpp COPYONLY IMMEDIATE)

set(TEST_NAMES
  H5LiteTest
  H5UtilitiesTest
  )


set( ${PLUGIN_NAME}_TEST_SRCS )
set(FilterTestIncludes "")
set(TestMainFunctors "")

foreach(name  ${TEST_NAMES})
  set( ${PLUGIN_NAME}_TEST_SRCS
    ${${PLUGIN_NAME}_TEST_SRCS}
    "${${PLUGIN_NAME}_SOURCE_DIR}/Test/${name}.cpp"
    )
  string(CONCAT
    FilterTestIncludes
    ${FilterTestIncludes}
    "#include \"${${PLUGIN_NAME}_SOURCE_DIR}/Test/${name}.cpp\"\n"
    )

  string(CONCAT
    TestMainFunctors
   ${TestMainFunctors}
   "  ${name}()()|\n")
endforeach()

STRING(REPLACE "|" ";" TestMainFunctors ${TestMainFunctors}   )

configure_file(${${PLUGIN_NAME}_SOURCE_DIR}/Test/${PLUGIN_NAME}TestMain.cpp.in
               ${${PLUGIN_NAME}Test_BINARY_DIR}/${PLUGIN_NAME}UnitTest.cpp @ONLY)

# Set the source files properties on each source file.
foreach(f ${${PLUGIN_NAME}_TEST_SRCS})
  set_source_files_properties( ${f} PROPERTIES HEADER_FILE_ONLY TRUE)
endforeach()

get_filename_component(TARGET_SOURCE_DIR_PARENT ${${PLUGIN_NAME}_SOURCE_DIR} PATH)
get_filename_component(TARGET_BINARY_DIR_PARENT ${${PLUGIN_NAME}_BINARY_DIR} PATH)


AddSIMPLUnitTest(TESTNAME H5SupportUnitTest
  SOURCES 
    ${${PLUGIN_NAME}Test_BINARY_DIR}/${PLUGIN_NAME}UnitTest.cpp 
    ${${PLUGIN_NAME}_TEST_SRCS}
  FOLDER 
    "H5SupportProj/Test"
  LINK_LIBRARIES 
    Qt5::Core 
    H5Support 
    SIMPLib
  INCLUDE_DIRS 
    ${TARGET_SOURCE_DIR_PARENT} 
    ${TARGET_BINARY_DIR_PARENT} 
    ${${PLUGIN_NAME}_SOURCE_DIR}/Test 
    ${${PLUGIN_NAME}_BINARY_DIR}/Test 
    ${${PLUGIN_NAME}Test_BINARY_DIR}
    ${SIMPLProj_SOURCE_DIR}/Source
  )

if(MSVC)
  set_source_files_properties( ${${PLUGIN_NAME}Test_BINARY_DIR}/${PLUGIN_NAME}UnitTest.cpp PROPERTIES COMPILE_FLAGS /bigobj)
endif()


if(0)
  # This is just a quick test to make sure that the latest HDF5 can actually write data
  # sets that are larger than 4GB in size
  add_executable(BigHDF5DatasetTest ${DREAM3DTest_SOURCE_DIR}/BigHDF5DatasetTest.cpp)
  target_link_libraries(BigHDF5DatasetTest Qt5::Core H5Support )
  set_target_properties(BigHDF5DatasetTest PROPERTIES FOLDER Test)
  add_test(BigHDF5DatasetTest ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/BigHDF5DatasetTest)


endif()
