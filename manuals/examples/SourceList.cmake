
#---------------------------------------------------------------------
# Include all the subdirectories that should be packaged.
include(${EMsoft_SOURCE_DIR}/manuals/examples/EBSDPatterns/SourceList.cmake)
include(${EMsoft_SOURCE_DIR}/manuals/examples/KosselPatterns/SourceList.cmake)

file(COPY "${EMsoft_SOURCE_DIR}/manuals/examples" DESTINATION "${PROJECT_BINARY_DIR}/")
