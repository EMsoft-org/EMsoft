
include ("@CMP_SOURCE_DIR@/cmpCMakeMacros.cmake")

#------------------------------------------------------------------------------
# Generate Fortran module and C++ header file that contains all the String constants
set(FORTRAN_STRING_CONSTANTS "")
set(CPP_STRING_CONSTANTS "")
if(1)
  set(EMSOFT_STRING_CLASS_INCLUDE "<QtCore/QString>")
  set(EMSOFT_STRING_CLASS "QString")
else()
  set(EMSOFT_STRING_CLASS_INCLUDE "<string>")
  set(EMSOFT_STRING_CLASS "std::string")
endif()

file(STRINGS "@EMsoftLib_SOURCE_DIR@/stringconstants.in" EMSOFT_STRING_CONSTANTS)
foreach(SC ${EMSOFT_STRING_CONSTANTS})
  string(FIND "${SC}" "!" COMMENT_POS)
  list(LENGTH SC LIST_LENGTH)
  string(FIND "${SC}" "[CATEGORY]" CATEGORY_POS)
  if(NOT CATEGORY_POS EQUAL -1)
    list(GET SC 1  VAR_VALUE)
    #message(STATUS "FOUND CATEGORY: ${VAR_VALUE}")
    set(FORTRAN_STRING_CONSTANTS 
      ${FORTRAN_STRING_CONSTANTS}
      "\n!------------------------------------------\n"
      "! ${VAR_VALUE}\n"
      "!------------------------------------------\n"
      )
    set(CPP_STRING_CONSTANTS 
      ${CPP_STRING_CONSTANTS}
      "\n    /* ----------------------------------------  */\n"
      "    /* ------------- ${VAR_VALUE} -------- */\n"
      "    /* ----------------------------------------  */\n"
      )
  elseif(COMMENT_POS EQUAL -1 AND LIST_LENGTH EQUAL 2)
    # CHECK FOR LIST LENGTH = 2
    list(GET SC 0  VAR_NAME)
    list(GET SC 1  VAR_VALUE)
    string(LENGTH ${VAR_VALUE} VAR_STR_LEN)
    math(EXPR VAR_STR_LEN ${VAR_STR_LEN}-2)
    set(FORTRAN_STRING_CONSTANTS 
      ${FORTRAN_STRING_CONSTANTS}
      "character(${VAR_STR_LEN}), parameter     :: SC_${VAR_NAME} = ${VAR_VALUE}\n!DEC$ ATTRIBUTES DLLEXPORT :: SC_${VAR_NAME}\n"
      )
    set(CPP_STRING_CONSTANTS 
      ${CPP_STRING_CONSTANTS}
      "    const ${EMSOFT_STRING_CLASS} ${VAR_NAME}(${VAR_VALUE})!\n"
      )

  endif()


  endif()
endforeach()


string(REPLACE  ";" "" FORTRAN_STRING_CONSTANTS ${FORTRAN_STRING_CONSTANTS})
cmpConfigureFileWithMD5Check(CONFIGURED_TEMPLATE_PATH "@EMsoftLib_SOURCE_DIR@/stringconstants.f90.in"
                             GENERATED_FILE_PATH "@EMsoftLib_BINARY_DIR@/stringconstants.f90"
                             VERBOSE TRUE)

string(REPLACE  ";" "" CPP_STRING_CONSTANTS ${CPP_STRING_CONSTANTS})
string(REPLACE  "!" ";" CPP_STRING_CONSTANTS ${CPP_STRING_CONSTANTS})
cmpConfigureFileWithMD5Check(CONFIGURED_TEMPLATE_PATH "@EMsoftLib_SOURCE_DIR@/EMsoftStringConstants.h.in"
                             GENERATED_FILE_PATH "@EMsoftLib_BINARY_DIR@/EMsoftStringConstants.h"
                             VERBOSE TRUE)

