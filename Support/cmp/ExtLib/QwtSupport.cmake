# --------------------------------------------------------------------
#-- Copy all the dependent DLLs into the current build directory so that the test
#-- can run.
function(AddQwtCopyInstallRules)
	set(options)
	set(oneValueArgs CMAKE_VAR PREFIX)
	set(multiValueArgs)

	cmake_parse_arguments(P "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )
	# message(STATUS "Copying Qwt Runtime Libraries: ${P_LIBRARIES}")
	set(SUPPORT_LIB_OPTION 1)
	if(MSVC_IDE)
		set(SUPPORT_LIB_OPTION 0)
	elseif(APPLE) # Apple systems do NOT need this so just skip this entirely
		set(SUPPORT_LIB_OPTION 2)
	elseif(UNIX AND NOT MSVC)
		set(SUPPORT_LIB_OPTION 3) # This should be Linux systems
	endif()

	# message(STATUS "AddQwtCopyInstallRules SUPPORT_LIB_OPTION  = ${SUPPORT_LIB_OPTION}")

	if(WIN32)
		set(destination "./")
	else()
		set(destination "lib")
	endif()

	set(TYPES Debug Release)
	if( ${SUPPORT_LIB_OPTION} EQUAL 1)
		set(TYPES ${CMAKE_BUILD_TYPE})
	endif()

	if(SUPPORT_LIB_OPTION EQUAL 0)

		file(MAKE_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Debug)
		file(MAKE_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Release)
		file(MAKE_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/MinSizeRel)
		file(MAKE_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/RelWithDebInfo)

		# Create a Copy/Install for the Debug Builds
		set(INT_DIR "Debug")
		set(DLL_VAR ${${P_CMAKE_VAR}_DLL_DEBUG})
	    #message(STATUS "====> DLL_VAR: ${DLL_VAR}")

		# message(STATUS "Copy Rule for Library:  ${DLL_VAR}")
		ADD_CUSTOM_TARGET(ZZ_${P_CMAKE_VAR}_DLL_${INT_DIR}-Copy ALL
		              COMMAND ${CMAKE_COMMAND} -E copy_if_different ${DLL_VAR}
		              ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/
		              COMMENT "  Copy: ${DLL_VAR}    To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/")
		set_target_properties(ZZ_${P_CMAKE_VAR}_DLL_${INT_DIR}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${INT_DIR}/Qwt)
		install(FILES ${DLL_VAR}  DESTINATION "${destination}" CONFIGURATIONS ${INT_DIR} COMPONENT Applications)

		# Create a Copy/Install for the Release Builds
		set(INT_DIR "Release")
		set(DLL_VAR ${${P_CMAKE_VAR}_DLL_RELEASE})
			
		# message(STATUS "Copy Rule for Library:  ${DLL_VAR}")
		ADD_CUSTOM_TARGET(ZZ_${P_CMAKE_VAR}_DLL_${INT_DIR}-Copy ALL
		              COMMAND ${CMAKE_COMMAND} -E copy_if_different ${DLL_VAR}
		              ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/
		              COMMENT "  Copy: ${DLL_VAR}    To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/")
		set_target_properties(ZZ_${P_CMAKE_VAR}_DLL_${INT_DIR}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${INT_DIR}/Qwt)
		install(FILES ${DLL_VAR}  DESTINATION "${destination}" CONFIGURATIONS ${INT_DIR} COMPONENT Applications)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${P_CMAKE_VAR}_DLL_${INT_DIR}-Copy) 
	elseif(SUPPORT_LIB_OPTION EQUAL 1)
		# This should happen for things like NMake and Ninja
		# Create a Copy/Install for the Debug Builds
		set(DLL_VAR ${${P_CMAKE_VAR}_DLL_DEBUG})
		set(INT_DIR ".")
		if( "${CMAKE_BUILD_TYPE}" STREQUAL "" OR "${CMAKE_BUILD_TYPE}" STREQUAL "Release")
			set(DLL_VAR ${${P_CMAKE_VAR}_DLL_RELEASE})
		endif()
		# message(STATUS "====> DLL_VAR: ${DLL_VAR}")
			
		# message(STATUS "Copy Rule for Library:  ${DLL_VAR}")
		ADD_CUSTOM_TARGET(ZZ_${P_CMAKE_VAR}_DLL_${INT_DIR}-Copy ALL
		              COMMAND ${CMAKE_COMMAND} -E copy_if_different ${DLL_VAR}
		              ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/
		              COMMENT "  Copy: ${DLL_VAR}    To: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${INT_DIR}/")
		set_target_properties(ZZ_${P_CMAKE_VAR}_DLL_${INT_DIR}-Copy PROPERTIES FOLDER ZZ_COPY_FILES/${INT_DIR}/Qwt)
		install(FILES ${DLL_VAR}  DESTINATION "${destination}" CONFIGURATIONS ${CMAKE_BUILD_TYPE} COMPONENT Applications)
        get_property(COPY_LIBRARY_TARGETS GLOBAL PROPERTY COPY_LIBRARY_TARGETS)
        set_property(GLOBAL PROPERTY COPY_LIBRARY_TARGETS ${COPY_LIBRARY_TARGETS} ZZ_${P_CMAKE_VAR}_DLL_${INT_DIR}-Copy) 
	elseif(SUPPORT_LIB_OPTION EQUAL 3)

		if(CMAKE_SYSTEM_NAME MATCHES "Linux")
		  GET_FILENAME_COMPONENT (SELF_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
			configure_file("${SELF_DIR}/Deploy_Qwt_Libs.sh.in"
		                 "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../AdditionalInstallScripts/Deploy_Qwt_Libs.sh" @ONLY IMMEDIATE)
		endif()
	endif()
endfunction()


# --------------------------------------------------------------------
# Override CMake's built in module directory by prepending cmp's module
# directory first

get_filename_component(PARENT_DIR ${CMAKE_CURRENT_LIST_DIR} DIRECTORY CACHE)

set(CMP_MODULES_SOURCE_DIR "${PARENT_DIR}/Modules")
set(CMAKE_MODULE_PATH ${CMP_MODULES_SOURCE_DIR} ${CMAKE_MODULE_PATH})

#------------------------------------------------------------------------------
# Make sure we have Qwt installed and available
find_package(Qwt)
if(QWT_FOUND)
    get_property(SIMPLibSearchDirs GLOBAL PROPERTY SIMPLibSearchDirs)
		file(APPEND "${SIMPLibSearchDirs}" "${QWT_LIB_DIR};")
    AddQwtCopyInstallRules(PREFIX "" CMAKE_VAR QWT_LIBRARY)
else()
    message(FATAL_ERROR "Qwt is required for this project")
endif()

