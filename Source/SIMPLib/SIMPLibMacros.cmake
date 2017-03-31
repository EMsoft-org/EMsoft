

#-------------------------------------------------------------------------------
# Function START_FILTER_GROUP
# @param ALL_FILTERS_HEADERFILE
# @param REGISTER_KNOWN_FILTERS_FILE
# @param FILTER_GROUP
# @param BINARY_DIR
function(SIMPL_START_FILTER_GROUP)

  set(options)
  set(oneValueArgs ALL_FILTERS_HEADERFILE REGISTER_KNOWN_FILTERS_FILE FILTER_GROUP BINARY_DIR)
  set(multiValueArgs)

  cmake_parse_arguments(P "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  file(APPEND ${P_ALL_FILTERS_HEADERFILE} "\n/* ------ ${P_FILTER_GROUP} --------- */\n")
  file(APPEND ${P_REGISTER_KNOWN_FILTERS_FILE} "\n    /* ------ ${P_FILTER_GROUP} --------- */\n")

  set_property(GLOBAL APPEND_STRING PROPERTY ${P_FILTER_GROUP}_ALL_FILTERS_HEADER "\n/* ------ ${P_FILTER_GROUP} --------- */\n")
  set_property(GLOBAL APPEND_STRING PROPERTY ${P_FILTER_GROUP}_REGISTER_KNOWN_FILTERS "\n    /* ------ ${P_FILTER_GROUP} --------- */\n")

  set_property(GLOBAL APPEND PROPERTY DREAM3DDoc_GROUPS ${P_FILTER_GROUP})
endfunction()

#-------------------------------------------------------------------------------
# Macro END_FILTER_GROUP
# @param WidgetsBinaryDir
# @param filterGroup
# @param humanGroup
macro(SIMPL_END_FILTER_GROUP WidgetsBinaryDir filterGroup humanGroup)
endmacro()

#-------------------------------------------------------------------------------
# Macro ADD_SIMPL_SUPPORT_HEADER
macro(ADD_SIMPL_SUPPORT_HEADER SourceDir filterGroup headerFileName)
    set(Project_SRCS ${Project_SRCS}
                    ${SourceDir}/${filterGroup}/${headerFileName})
    cmp_IDE_SOURCE_PROPERTIES( "${filterGroup}" "${SourceDir}/${filterGroup}/${headerFileName}" "" "0")
endmacro()

#-------------------------------------------------------------------------------
# Macro ADD_SIMPL_SUPPORT_MOC_HEADER
macro(ADD_SIMPL_SUPPORT_MOC_HEADER SourceDir filterGroup headerFileName)
  QT5_WRAP_CPP( _moc_filter_source  ${SourceDir}/${filterGroup}/${headerFileName})
  set_source_files_properties( ${_moc_filter_source} PROPERTIES GENERATED TRUE)
  set_source_files_properties( ${_moc_filter_source} PROPERTIES HEADER_FILE_ONLY TRUE)

  set(Project_SRCS ${Project_SRCS}
                    ${SourceDir}/${filterGroup}/${headerFileName}
                    ${_moc_filter_source})
  cmp_IDE_SOURCE_PROPERTIES( "${filterGroup}" "${SourceDir}/${filterGroup}/${headerFileName}" "" "0")
endmacro()

#-------------------------------------------------------------------------------
# Macro ADD_SIMPL_SUPPORT_HEADER_SUBDIR
macro(ADD_SIMPL_SUPPORT_HEADER_SUBDIR SourceDir filterGroup headerFileName subdir)
    set(Project_SRCS ${Project_SRCS}
                    ${SourceDir}/${filterGroup}/${subdir}/${headerFileName})
    cmp_IDE_SOURCE_PROPERTIES( "${filterGroup}/${subdir}" "${SourceDir}/${filterGroup}/${subdir}/${headerFileName}" "" "0")
endmacro()

#-------------------------------------------------------------------------------
# Macro ADD_SIMPL_SUPPORT_SOURCE
macro(ADD_SIMPL_SUPPORT_SOURCE SourceDir filterGroup sourceFileName)
    set(Project_SRCS ${Project_SRCS}
                    ${SourceDir}/${filterGroup}/${sourceFileName})
    cmp_IDE_SOURCE_PROPERTIES( "${filterGroup}" "" "${SourceDir}/${filterGroup}/${sourceFileName}" "0")
endmacro()
#-------------------------------------------------------------------------------
# Macro ADD_SIMPL_SUPPORT_CLASS
macro(ADD_SIMPL_SUPPORT_CLASS SourceDir filterGroup className)
    set(Project_SRCS ${Project_SRCS}
                    ${SourceDir}/${filterGroup}/${className}.h
                    ${SourceDir}/${filterGroup}/${className}.cpp)
    cmp_IDE_SOURCE_PROPERTIES( "${filterGroup}" "${SourceDir}/${filterGroup}/${className}.h" "${SourceDir}/${filterGroup}/${className}.cpp" "0")
endmacro()


#-------------------------------------------------------------------------------
# Macro ADD_SIMPL_FILTER
# @param FilterLib The Library/Plugin the filter belongs to.
# @param WidgetLib The Widgets library the filter belongs to
# @param filterGroup The group or plugin that the filter belongs to.
# @param filterName The base filename of the filter, i.e., SomeFilter.cpp would be "SomeFilter"
# @param filterDocPath The absolute path to the .md documentation file
# @param publicFilter  Boolean TRUE or FALSE
macro(ADD_SIMPL_FILTER FilterLib WidgetLib filterGroup filterName filterDocPath publicFilter)

  QT5_WRAP_CPP( _moc_filter_source  ${${FilterLib}_SOURCE_DIR}/${filterGroup}/${filterName}.h)
  set_source_files_properties( ${_moc_filter_source} PROPERTIES GENERATED TRUE)
  set_source_files_properties( ${_moc_filter_source} PROPERTIES HEADER_FILE_ONLY TRUE)

  set(Project_SRCS ${Project_SRCS}
                  ${${FilterLib}_SOURCE_DIR}/${filterGroup}/${filterName}.h
                  ${${FilterLib}_SOURCE_DIR}/${filterGroup}/${filterName}.cpp
                  ${_moc_filter_source}
                  )
  #--- Organize inside the Visual Studio/Xcode Projects
  cmp_IDE_SOURCE_PROPERTIES( "${filterGroup}" "${${FilterLib}_SOURCE_DIR}/${filterGroup}/${filterName}.h" "${${FilterLib}_SOURCE_DIR}/${filterGroup}/${filterName}.cpp" ${PROJECT_INSTALL_HEADERS})
  cmp_IDE_GENERATED_PROPERTIES ( "Generated/${FilterLib}/${filterGroup}" "" "${_moc_filter_source}" "0")

  #-- Create an Install Rule for the headers
#  if( ${PROJECT_INSTALL_HEADERS} EQUAL 1 )
#      INSTALL (FILES ${${FilterLib}_SOURCE_DIR}/${filterGroup}/${filterName}.h
#          DESTINATION include/${FilterLib}/${filterGroup}
#          COMPONENT Headers)
#  endif()

  file(APPEND ${AllFiltersHeaderFile} "#include \"${FilterLib}/${filterGroup}/${filterName}.h\"\n")

  if( ${publicFilter} STREQUAL TRUE)
      file(APPEND ${RegisterKnownFiltersFile} "   FilterFactory<${filterName}>::Pointer ${filterName}Factory = FilterFactory<${filterName}>::New();\n")
      file(APPEND ${RegisterKnownFiltersFile} "   fm->addFilterFactory(\"${filterName}\",${filterName}Factory);\n\n")

      #-- Check to make sure we have a Documentation file for the filter
      if(NOT EXISTS ${filterDocPath} )
        message(FATAL_ERROR "*** Missing Documenation File for ${filterDocPath}")
      endif()

      get_property(DREAM3DDocRoot GLOBAL PROPERTY DREAM3DDocRoot)
      #file(APPEND ${DREAM3DDocRoot}/DREAM3DDoc_${filterGroup} "${filterDocPath}\n")
      set_property(GLOBAL APPEND PROPERTY DREAM3DDoc_${filterGroup} ${filterDocPath})

  endif()
endmacro()

#-------------------------------------------------------------------------------
# Macro ADD_FILTER_LIST
macro(ADD_FILTER_LIST)

        file(APPEND ${RegisterKnownFiltersFile} "\tQList<QString> pluginList;\n\n")

    foreach(f ${_PublicFilters} )
        file(APPEND ${RegisterKnownFiltersFile} "\tpluginList.append(\"${f}\");\n")
    endforeach()

    file(APPEND ${RegisterKnownFiltersFile} "\n\treturn pluginList;")

endmacro()
