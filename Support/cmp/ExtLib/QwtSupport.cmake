#--////////////////////////////////////////////////////////////////////////////
# Copyright (c) 2009-2015 BlueQuartz Software, LLC
#
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright notice, this
# list of conditions and the following disclaimer in the documentation and/or
# other materials provided with the distribution.
#
# Neither the name of BlueQuartz Software, the US Air Force, nor the names of its
# contributors may be used to endorse or promote products derived from this software
# without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
# USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The code contained herein was partially funded by the followig contracts:
#    United States Air Force Prime Contract FA8650-07-D-5800
#    United States Air Force Prime Contract FA8650-10-D-5210
#    United States Prime Contract Navy N00173-07-C-2068
#--////////////////////////////////////////////////////////////////////////////


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
	elseif(SUPPORT_LIB_OPTION EQUAL 3)

		if(CMAKE_SYSTEM_NAME MATCHES "Linux")
		  GET_FILENAME_COMPONENT (SELF_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
			configure_file("${SELF_DIR}/Deploy_Qwt_Libs.sh.in"
		                 "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../AdditionalInstallScripts/Deploy_Qwt_Libs.sh" @ONLY IMMEDIATE)
		endif()
	endif()
endfunction()


#------------------------------------------------------------------------------
# Make sure we have Qwt installed and available
find_package(Qwt)
if(QWT_FOUND)
    include_directories(${QWT_INCLUDE_DIR})
    get_property(SIMPLibSearchDirs GLOBAL PROPERTY SIMPLibSearchDirs)
		file(APPEND "${SIMPLibSearchDirs}" "${QWT_LIB_DIR};")
    AddQwtCopyInstallRules(PREFIX "" CMAKE_VAR QWT_LIBRARY)
else()
    message(FATAL_ERROR "Qwt is required for this project")
endif()

