set(DictionaryIndexing_Wrapper_SRCS
 ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/EMDIwrappermod.f90
 ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/EMsoftDIwrappersCallbacks.c
 ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/PyUtils.cpp
#  ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/PyEMEBSDDI.cpp
)
set(DictionaryIndexing_Wrapper_HEADERS
  ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/EMsoftDIwrappers.h
  ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/EMsoftDIwrappersCallbacks.h
  ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/PyUtils.hpp
)

# find python executable
find_package(PythonInterp 3 REQUIRED)
find_package(PythonLibs 3 REQUIRED)

# find python include directory
execute_process(COMMAND ${PYTHON_EXECUTABLE} "-c" "from distutils import sysconfig;print(sysconfig.get_python_inc(),end='')" OUTPUT_VARIABLE Python_Output ERROR_VARIABLE Python_Error)
STRING(REGEX REPLACE "\\\\" "/" Python_Output ${Python_Output}) # convert windows backslash to forward slash
if(NOT "${Python_Error}" STREQUAL "")
    message(FATAL_ERROR "failed to find python include directory: ${Python_Error}")
endif()
set(PYTHON_INCLUDE_DIR ${Python_Output} CACHE PATH "python include directories")

# find python library
if(WIN32)
    execute_process(COMMAND ${PYTHON_EXECUTABLE} "-c" "from distutils import sysconfig;print(sysconfig.get_config_var(\"BINDIR\"),end='')" OUTPUT_VARIABLE Python_Output ERROR_VARIABLE Python_Error)
    STRING(REGEX REPLACE "\\\\" "/" Python_Output ${Python_Output})
    if(NOT "${Python_Error}" STREQUAL "")
        message(FATAL_ERROR "failed to find python include directory: ${Python_Error}")
    endif()
    set(PYTHON_LIBRARY_DIR ${Python_Output}/libs CACHE PATH "python library directories")

    if(MSVC)
        find_library(PYTHON_LIBRARY    python${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR} HINT ${PYTHON_LIBRARY_DIR} CACHE FILE)
    elseif(MINGW)
        find_library(PYTHON_LIBRARY libpython${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR} HINT ${PYTHON_LIBRARY_DIR} CACHE FILE)
    endif()
else()
    execute_process(COMMAND ${PYTHON_EXECUTABLE} "-c" "from distutils import sysconfig;print(sysconfig.get_config_var(\"LIBDIR\"),end='')" OUTPUT_VARIABLE Python_Output ERROR_VARIABLE Python_Error)
    if(NOT "${Python_Error}" STREQUAL "")
        message(FATAL_ERROR "failed to find python include directory: ${Python_Error}")
    endif()
    set(PYTHON_LIBRARY_DIR ${Python_Output} CACHE PATH "python library directories")
    find_library(PYTHON_LIBRARY libpython${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}m.dylib HINTS ${PYTHON_LIBRARY_DIR} CACHE FILE)
endif()

# find numpy
execute_process(COMMAND ${PYTHON_EXECUTABLE} "-c" "import numpy;print(numpy.get_include(),end='')" OUTPUT_VARIABLE Python_Output ERROR_VARIABLE Python_Error)
STRING(REGEX REPLACE "\\\\" "/" Python_Output ${Python_Output})
if(NOT "${Python_Error}" STREQUAL "")
    message(FATAL_ERROR "failed to find numpy include directory: ${Python_Error}")
endif()
find_path(NUMPY_INCLUDE_DIR numpy/arrayobject.h HINTS ${Python_Output})
get_filename_component(NUMPY_DIR ${NUMPY_INCLUDE_DIR} DIRECTORY)
find_library(NUMPY_LIBRARY NAMES npymath HINTS ${NUMPY_DIR}/lib)
get_filename_component(NUMPY_LIBRARY_DIR ${NUMPY_LIBRARY} DIRECTORY CACHE)


# add library
add_library(PyEMEBSDDI MODULE ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/PyEMEBSDDI.cpp)

target_compile_options(PyEMEBSDDI PRIVATE -fPIC)
target_link_libraries(PyEMEBSDDI EMsoftWrapperLib EMsoftHDFLib EMOpenCLLib)
target_link_libraries(PyEMEBSDDI ${PYTHON_LIBRARY} ${NUMPY_LIBRARY})
target_include_directories(PyEMEBSDDI
      PUBLIC ${FFTW3_INCLUDE_DIR} 
      PUBLIC ${CLFORTRAN_INCLUDE_DIR}
      ${CLFortran_INSTALL}/include
      $<BUILD_INTERFACE:${EMsoftHDFLib_BINARY_DIR}>
      $<BUILD_INTERFACE:${EMOpenCLLib_BINARY_DIR}>
      PUBLIC ${PYTHON_INCLUDE_DIR}
      PUBLIC ${NUMPY_INCLUDE_DIR}
)

set_property(TARGET PyEMEBSDDI PROPERTY CXX_STANDARD 11)
set_target_properties(
    PyEMEBSDDI
    PROPERTIES
        OUTPUT_NAME "PyEMEBSDDI"
        LINKER_LANGUAGE C
    )

set_property(TARGET PyEMEBSDDI PROPERTY PREFIX "") # name crystallography instead of libcrystallography
if(WIN32)
    set_property(TARGET PyEMEBSDDI PROPERTY SUFFIX ".pyd") # name crystallography.pyd instead of crystallography.dll
else()
    set_property(TARGET PyEMEBSDDI PROPERTY SUFFIX ".so") # name crystallography.so
endif()

# add install rule to put python module in python path
execute_process(COMMAND ${PYTHON_EXECUTABLE} "-c" "from distutils import sysconfig;print(sysconfig.get_python_lib(),end='')" OUTPUT_VARIABLE Python_Output ERROR_VARIABLE Python_Error)
if(NOT "${Python_Error}" STREQUAL "")
	message(FATAL_ERROR "failed to find python include directory: ${Python_Error}")
endif()
set(PYTHON_MODULE_DIR ${Python_Output} CACHE PATH "python module install directory")
install(TARGETS PyEMEBSDDI 
    LIBRARY DESTINATION ${PYTHON_MODULE_DIR})
