#!/bin/bash


#------------------------------------------------------------------------------
# Read the configuration file for the SDK Build. All important variables are 
# stored in the .conf file. DO NOT CHANGE variables in this file.
shopt -s extglob
configfile="SDK_Configuration.conf" # set the actual path name of your (DOS or Unix) config file
tr -d '\r' < $configfile > $configfile.unix
while IFS='= ' read lhs rhs
do
    if [[ ! $lhs =~ ^\ *# && -n $lhs ]]; then
        rhs="${rhs%%\#*}"    # Del in line right comments
        rhs="${rhs%%*( )}"   # Del trailing spaces
        rhs="${rhs%\"*}"     # Del opening string quotes 
        rhs="${rhs#\"*}"     # Del closing string quotes 
        declare $lhs="$rhs"
    fi
done < $configfile.unix
rm $configfile.unix
#------------------------------------------------------------------------------


echo "SDK_INSTALL=$SDK_INSTALL"
echo "PARALLEL_BUILD=$PARALLEL_BUILD"

HOST_SYSTEM=`uname`
echo "Host System: $HOST_SYSTEM"


SCRIPT_DIR=`pwd`

if [ ! -e "$SDK_PARENT/$SDK_ARCHIVE_FILENAME" ];
  then
  echo "-------------------------------------------"
  echo " Downloading EMsoft SDK Source Archive "
  echo "-------------------------------------------"
  $DOWNLOAD_PROG  "$SDK_DOWNLOAD_SITE/$SDK_ARCHIVE_FILENAME" -o $SDK_PARENT/$SDK_ARCHIVE_FILENAME
fi

#-------------------------------------------------
# Move one Directory Above the SDK Folder and untar the
if [ -e "$SDK_PARENT/$SDK_ARCHIVE_FILENAME" ];
  then
  echo "Decompressing Archive $SDK_PARENT/$SDK_ARCHIVE_FILENAME"
   mkdir -p ${SDK_INSTALL}
   chmod ugo+rwx ${SDK_INSTALL}
  cd "$SDK_INSTALL/../"
   tar -xvzf $SDK_ARCHIVE_FILENAME
   chmod ugo+rwx $SDK_INSTALL
  USER=`whoami`
   chown -R ${USER} "$SDK_INSTALL"
fi

#-------------------------------------------------
# Move into the SDK directory
cd ${SDK_INSTALL}

#-------------------------------------------------
# Unpack CMake
 tar -xvzf ${SDK_INSTALL}/cmake-3.4.3-${HOST_SYSTEM}-x86_64.tar.gz

#-------------------------------------------------
# Get CMake on our path
if [[ "$HOST_SYSTEM" = "Darwin" ]];
then
  export PATH=$PATH:${SDK_INSTALL}/cmake-3.4.3-${HOST_SYSTEM}-x86_64/CMake.app/Contents/bin
else
  export PATH=$PATH:${SDK_INSTALL}/cmake-3.4.3-${HOST_SYSTEM}-x86_64/bin
fi

#-------------------------------------------------
# Create the EMsoft_SDK.cmake file, but back up any existing one first
if [ -e "$SDK_INSTALL/EMsoft_SDK.cmake" ]
  then
   mv "$SDK_INSTALL/EMsoft_SDK.cmake" "$SDK_INSTALL/EMsoft_SDK.cmake.bak"
fi
echo "# This is the EMsoft_SDK File. This file contains all the paths to the dependent libraries." > "$SDK_INSTALL/EMsoft_SDK.cmake"

echo "if(NOT DEFINED EMsoft_FIRST_CONFIGURE)" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "  message(STATUS \"*******************************************************\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "  message(STATUS \"* EMsoft First Configuration Run                    *\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "  message(STATUS \"* EMsoft_SDK Loading from \${CMAKE_CURRENT_LIST_DIR}  *\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "  message(STATUS \"*******************************************************\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "endif()" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(CMAKE_CXX_FLAGS \"-Wno-four-char-constants -Wno-unknown-pragmas -mfpmath=sse\" CACHE STRING \"\" FORCE)" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "#--------------------------------------------------------------------------------------------------" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# These settings are specific to EMsoft. EMsoft needs these variables to" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# configure properly." >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(BUILD_TYPE \${CMAKE_BUILD_TYPE})" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "if(\"\${BUILD_TYPE}\" STREQUAL \"\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "    set(BUILD_TYPE \"Release\" CACHE STRING \"\" FORCE)" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "endif()" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "message(STATUS \"The Current Build type being used is \${BUILD_TYPE}\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(BUILD_SHARED_LIBS ON CACHE BOOL \"\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(EMsoft_SDK_ROOT \"$SDK_INSTALL\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(EMsoft_DATA_DIR \${EMsoft_SDK_ROOT}/EMsoft_Data CACHE PATH \"\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"

# Write out the Qt5 directory/installation
echo "#--------------------------------------------------------------------------------------------------" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# Currently EMsoft does not Depend on Qt5, but if it did, this line is needed." >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# Qt 5.5.1 Library" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# set(Qt5_DIR \"\${EMsoft_SDK_ROOT}/Qt5.5.1/5.5/clang_64/lib/cmake/Qt5\" CACHE PATH \"\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo ""  >> "$SDK_INSTALL/EMsoft_SDK.cmake"

#-------------------------------------------------
# Start building all the packages
cd $SCRIPT_DIR

#export FC=/usr/bin/gfortran-5

#-------------------------------------------------
# Build FFTW Library
 ./Build_FFTW3.sh "${SDK_INSTALL}" ${PARALLEL_BUILD}

#-------------------------------------------------
# Build CLFortran
 ./Build_CLFortran.sh "${SDK_INSTALL}" ${PARALLEL_BUILD}

#-------------------------------------------------
# Build JsonFortran Library
 ./Build_JsonFortran.sh "${SDK_INSTALL}" ${PARALLEL_BUILD}

#-------------------------------------------------
# Build HDF5 Library
 ./Build_HDF5.sh "${SDK_INSTALL}" ${PARALLEL_BUILD}

# Continue writing the EMsoft_SDK.cmake file after all those libraries were compiled
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "#--------------------------------------------------------------------------------------------------" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# Update CMake Module Path with additional paths in order to better find the libraries." >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(CMAKE_MODULE_PATH \${CMAKE_MODULE_PATH} \${HDF5_DIR} \${JSONFORTRAN_DIR})" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "#--------------------------------------------------------------------------------------------------" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# Only Run this the first time when configuring EMsoft. After that the values" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# are cached properly and the user can add additional plugins through the normal" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# CMake GUI or CCMake programs." >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "if(NOT DEFINED EMsoft_FIRST_CONFIGURE)" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "  set(EMsoft_FIRST_CONFIGURE \"ON\" CACHE STRING \"Determines if EMsoft has already been configured\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "endif()" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"


 chmod -R ugo+rw *


# Only look for Doxygen.app on OS X systems.
if [[ "$HOST_SYSTEM" = "Darwin" ]];
then
  if [ ! -e "/Applications/Doxygen.app" ];
  then
    echo "--------------------------------------------"
    echo "Doxygen is missing from your system."
    echo "Downloading Doxygen 1.8.10 for you."
     $DOWNLOAD_PROG  "http://ftp.stack.nl/pub/users/dimitri/Doxygen-1.8.11.dmg" -o "${EMsoft_SDK}/Doxygen-1.8.11.dmg"
    open "${EMsoft_SDK}/Doxygen-1.8.11.dmg"
    echo "Please Copy the Doxygen.app from the mounted disk image into the /Applications directory. CMake can most"
    echo "easily find it in this location."
  fi
fi
