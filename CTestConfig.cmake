
## This file should be placed in the root directory of your project.
## Then modify the CMakeLists.txt file in the root directory of your
## project to incorporate the testing dashboard.
## # The following are required to uses Dart and the Cdash dashboard
##   ENABLE_TESTING()
##   INCLUDE(CTest)
set(CTEST_PROJECT_NAME "EMsoft")
set(CTEST_NIGHTLY_START_TIME "02:00:00 UTC")

set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "my.cdash.org")
set(CTEST_DROP_LOCATION "/submit.php?project=EMsoft")
set(CTEST_DROP_SITE_CDASH TRUE)

# Use multiple CPU cores to build
include(ProcessorCount)
ProcessorCount(N)
if(NOT N EQUAL 0)
  if(NOT CTEST_CMAKE_GENERATOR STREQUAL "NMake Makefiles")
    if(CTEST_CMAKE_GENERATOR MATCHES "Make" OR CTEST_CMAKE_GENERATOR MATCHES "Ninja")
      set(CTEST_BUILD_FLAGS "-j${N}")
    endif()
  endif()
  set(ctest_test_args ${ctest_test_args} PARALLEL_LEVEL ${N})
endif()


