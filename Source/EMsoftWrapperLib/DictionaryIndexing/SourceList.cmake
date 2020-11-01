set(DictionaryIndexing_Wrapper_SRCS
 ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/EMDIwrappermod.f90
)
set(DictionaryIndexing_Wrapper_HEADERS
  ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/EMsoftDIwrappers.h
)


option(EMsoft_ENABLE_PyEMEBSDDI "Build sources and programs related to PyEMEBSDDI" OFF)

# add library
set(PyEMEBSDDI_SRCS
    ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/PyEMEBSDDI.cpp
    ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/PyUtils.cpp
    ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/EMsoftDIwrappersCallbacks.c
)
set(PyEMEBSDDI_HEADERS
    ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/PyUtils.hpp
    ${EMsoftWrapperLib_SOURCE_DIR}/DictionaryIndexing/EMsoftDIwrappersCallbacks.h
)

if (EMsoft_ENABLE_PyEMEBSDDI)
    find_package(Python3 COMPONENTS Interpreter Development NumPy REQUIRED)
    set(BUILD_SHARED_LIBS ON CACHE BOOL "Build shared libraries" FORCE)

    add_library(PyEMEBSDDI MODULE ${PyEMEBSDDI_SRCS} ${PyEMEBSDDI_HEADERS})

    target_compile_options(PyEMEBSDDI PRIVATE -fPIC)
    target_link_libraries(PyEMEBSDDI EMsoftWrapperLib EMsoftHDFLib EMOpenCLLib)
    target_link_libraries(PyEMEBSDDI Python3::Python Python3::NumPy)
    target_include_directories(PyEMEBSDDI
        PUBLIC ${FFTW3_INCLUDE_DIR} 
        PUBLIC ${CLFORTRAN_INCLUDE_DIR}
        ${CLFortran_INSTALL}/include
        $<BUILD_INTERFACE:${EMsoftHDFLib_BINARY_DIR}>
        $<BUILD_INTERFACE:${EMOpenCLLib_BINARY_DIR}>
        PUBLIC ${PYTHON3_INCLUDE_DIR}
        PUBLIC ${PYTHON3_NUMPY_INCLUDE_DIR}
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
    elseif(APPLE)
        set_property(TARGET PyEMEBSDDI PROPERTY SUFFIX ".dylib") # name crystallography.so
    else()
        set_property(TARGET PyEMEBSDDI PROPERTY SUFFIX ".so") # name crystallography.so

    endif()

    # install PyEMEBSDDI to Python Third-party platform independent installation directory
    install(TARGETS PyEMEBSDDI LIBRARY DESTINATION ${Python3_SITELIB})
endif()
