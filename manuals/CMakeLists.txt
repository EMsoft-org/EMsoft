



function(ADD_EMSOFT_LATEX_TARGETS)
  set(options )
  set(oneValueArgs PDF_FILENAME PROJECT_NAME PROJECT_BINARY_DIR OUTPUT_DIR FIGURES_DIR SOURCE_FILE CITATIONS_FILE INCLUDE_DIR)
  set(multiValueArgs EPS_FILES )
  cmake_parse_arguments(LTX "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
  if("${LTX_OUTPUT_DIR}" STREQUAL "")
    message(FATAL_ERROR "LTX_OUTPUT_DIR Variable MUST be set and it is empty")
  endif()

  #-- Create the proper Directory Structure
  #file(MAKE_DIRECTORY ${LTX_PROJECT_BINARY_DIR}/latex )

  #-- Set some additional Directories to remove when cleaning the project (Not sure if this actually works)
  set(ADDITIONAL_MAKE_CLEAN_FILES ${ADDITIONAL_MAKE_CLEAN_FILES} ${LTX_PROJECT_BINARY_DIR}/latex )

  #-- Setup some additional LaTex processor arguments
  SET(LATEX_COMPILER_FLAGS "--synctex=-1 --interaction nonstopmode --shell-escape" CACHE STRING "Flags passed to latex.")
  SET(PDFLATEX_COMPILER_FLAGS ${LATEX_COMPILER_FLAGS} CACHE STRING "Flags passed to pdflatex.")

  #-- Create more directory structures
  file(MAKE_DIRECTORY ${LTX_PROJECT_BINARY_DIR})

  #-- 
  set(LATEX_MAIN_INPUT ${LTX_SOURCE_FILE})

  #-- Copy Source LaTeX files
  get_filename_component(IMAGE_DIR_NAME "${LTX_FIGURES_DIR}" NAME)
  get_filename_component(TEX_FILE_NAME "${LTX_SOURCE_FILE}" NAME_WE)
  get_filename_component(BIB_FILE_NAME "${LTX_CITATIONS_FILE}" NAME_WE)
  get_filename_component(INCLUDE_DIR_NAME "${LTX_INCLUDE_DIR}" NAME_WE)
  file(COPY "${LTX_SOURCE_FILE}" DESTINATION "${LTX_PROJECT_BINARY_DIR}/")
  file(COPY "${LTX_CITATIONS_FILE}" DESTINATION "${LTX_PROJECT_BINARY_DIR}/")
  file(COPY "${LTX_FIGURES_DIR}/" DESTINATION "${LTX_PROJECT_BINARY_DIR}/${IMAGE_DIR_NAME}")
  file(COPY "${LTX_INCLUDE_DIR}/" DESTINATION "${LTX_PROJECT_BINARY_DIR}/${INCLUDE_DIR_NAME}")

  add_custom_target(${LTX_PROJECT_NAME})

  add_custom_command(TARGET ${LTX_PROJECT_NAME} POST_BUILD
                    COMMAND ${PDFLATEX_COMPILER} ${PDFLATEX_COMPILER_FLAGS}
                   # -output-directory=${LTX_OUTPUT_DIR}
                    --jobname=${TEX_FILE_NAME}
                    "${LTX_PROJECT_BINARY_DIR}/${TEX_FILE_NAME}.tex"
                    WORKING_DIRECTORY ${LTX_PROJECT_BINARY_DIR}
                    COMMENT "======> ${LTX_PROJECT_NAME} Generating PDF - First Pass")


  add_custom_command(TARGET ${LTX_PROJECT_NAME} POST_BUILD
                    COMMAND ${MAKEINDEX_COMPILER} ${MAKEINDEX_COMPILER_FLAGS}
                    "${TEX_FILE_NAME}.tex"
                    WORKING_DIRECTORY ${LTX_PROJECT_BINARY_DIR}
                    COMMENT "======> ${LTX_PROJECT_NAME} Generating Index")

  add_custom_command(TARGET ${LTX_PROJECT_NAME} POST_BUILD
                    COMMAND ${PDFLATEX_COMPILER} ${PDFLATEX_COMPILER_FLAGS}
                    #-output-directory=${LTX_OUTPUT_DIR}
                    --jobname=${TEX_FILE_NAME}
                    "${LTX_PROJECT_BINARY_DIR}/${TEX_FILE_NAME}.tex"
                    WORKING_DIRECTORY ${LTX_PROJECT_BINARY_DIR}
                    COMMENT "======> ${LTX_PROJECT_NAME} Generating PDF - Second Pass")

  if(APPLE)
 #   if(AUTO_OPEN_PDF)
        add_custom_command(TARGET ${LTX_PROJECT_NAME} POST_BUILD
                          COMMAND /usr/bin/open ${LTX_PROJECT_BINARY_DIR}/${TEX_FILE_NAME}.pdf
                          COMMENT "Opening PDF file with default Viewer")
  #  endif()
  endif()

  #---------------------------------------------------------------------
  # Create the Installation Rules
  INSTALL(FILES "${LTX_PROJECT_BINARY_DIR}/${TEX_FILE_NAME}.pdf"
    COMPONENT Applications
    DESTINATION "manuals"
  )

endfunction()

OPTION(BUILD_EMSOFT_DOCS "Build Documentation Libraries" OFF)
if (BUILD_EMSOFT_DOCS)

  # --------------------------------------------------------------------
  # Find LaTeX on the system
  include(${PROJECT_SOURCE_DIR}/manuals/UseLatex.cmake)

  ADD_EMSOFT_LATEX_TARGETS(
    PROJECT_NAME "EMEBSD_Manual"
    PROJECT_BINARY_DIR "${PROJECT_BINARY_DIR}/manuals"
    SOURCE_FILE "${PROJECT_SOURCE_DIR}/manuals/EMEBSD.tex"
    FIGURES_DIR "${PROJECT_SOURCE_DIR}/manuals/figs"
    INCLUDE_DIR "${PROJECT_SOURCE_DIR}/manuals/includefiles"
    OUTPUT_DIR "${PROJECT_BINARY_DIR}/manuals/EMEBSD"
    CITATIONS_FILE "${PROJECT_SOURCE_DIR}/manuals/citations.bib"
  )

  ADD_EMSOFT_LATEX_TARGETS(
    PROJECT_NAME "EMIndexing_Manual"
    PROJECT_BINARY_DIR "${PROJECT_BINARY_DIR}/manuals"
    SOURCE_FILE "${PROJECT_SOURCE_DIR}/manuals/EMIndexing.tex"
    FIGURES_DIR "${PROJECT_SOURCE_DIR}/manuals/figs"
    INCLUDE_DIR "${PROJECT_SOURCE_DIR}/manuals/includefiles"
    OUTPUT_DIR "${PROJECT_BINARY_DIR}/manuals/EMIndexing"
    CITATIONS_FILE "${PROJECT_SOURCE_DIR}/manuals/citations.bib"
  )

  ADD_EMSOFT_LATEX_TARGETS(
    PROJECT_NAME "HDFsupport_Manual"
    PROJECT_BINARY_DIR "${PROJECT_BINARY_DIR}/manuals"
    SOURCE_FILE "${PROJECT_SOURCE_DIR}/manuals/HDFsupport.tex"
    FIGURES_DIR "${PROJECT_SOURCE_DIR}/manuals/figs"
    INCLUDE_DIR "${PROJECT_SOURCE_DIR}/manuals/includefiles"
    OUTPUT_DIR "${PROJECT_BINARY_DIR}/manuals/HDFsupport"
    CITATIONS_FILE "${PROJECT_SOURCE_DIR}/manuals/citations.bib"
  )

  ADD_EMSOFT_LATEX_TARGETS(
    PROJECT_NAME "EMsoft_Manual"
    PROJECT_BINARY_DIR "${PROJECT_BINARY_DIR}/manuals"
    SOURCE_FILE "${PROJECT_SOURCE_DIR}/manuals/EMsoft.tex"
    FIGURES_DIR "${PROJECT_SOURCE_DIR}/manuals/figs"
    INCLUDE_DIR "${PROJECT_SOURCE_DIR}/manuals/includefiles"
    OUTPUT_DIR "${PROJECT_BINARY_DIR}/manuals/EMsoft"
    CITATIONS_FILE "${PROJECT_SOURCE_DIR}/manuals/citations.bib"
  )

endif()


