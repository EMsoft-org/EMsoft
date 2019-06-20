
set(SUBDIR_NAME Math)

set(SIMPLib_${SUBDIR_NAME}_HDRS
  #${SIMPLib_SOURCE_DIR}/Math/GeometryMath.h
  ${SIMPLib_SOURCE_DIR}/Math/MatrixMath.h
  ${SIMPLib_SOURCE_DIR}/Math/QuaternionMath.hpp
  ${SIMPLib_SOURCE_DIR}/Math/ArrayHelpers.hpp
  ${SIMPLib_SOURCE_DIR}/Math/SIMPLibMath.h
)
set(SIMPLib_${SUBDIR_NAME}_SRCS
  #${SIMPLib_SOURCE_DIR}/Math/GeometryMath.cpp
  ${SIMPLib_SOURCE_DIR}/Math/MatrixMath.cpp
  ${SIMPLib_SOURCE_DIR}/Math/SIMPLibMath.cpp
)
cmp_IDE_SOURCE_PROPERTIES( "${SUBDIR_NAME}" "${SIMPLib_${SUBDIR_NAME}_HDRS};${SIMPLib_${SUBDIR_NAME}_Moc_HDRS}" "${SIMPLib_${SUBDIR_NAME}_SRCS}" "${PROJECT_INSTALL_HEADERS}")
cmp_IDE_SOURCE_PROPERTIES( "Generated/${SUBDIR_NAME}" "" "${SIMPLib_${SUBDIR_NAME}_Generated_MOC_SRCS}" "0")

if( ${PROJECT_INSTALL_HEADERS} EQUAL 1 )
    INSTALL (FILES ${SIMPLib_Math_HDRS}
            DESTINATION include/SIMPLib/Math
            COMPONENT Headers   )
endif()
