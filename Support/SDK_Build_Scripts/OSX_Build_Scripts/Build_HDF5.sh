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

cd $SDK_INSTALL

HOST_SYSTEM=`uname`
echo "Host System: $HOST_SYSTEM"


CMAKE=`type -P cmake`
if [[ $CMAKE == "" ]];
  then
  echo "CMake is needed for this script. Please install it on your system and be sure it is on your path."
  exit 1
fi

# Build the HDF5 libraries we need and set our Environment Variable.

if [ ! -e "$SDK_INSTALL/${HDF5_ARCHIVE_NAME}" ];
then
  echo "-------------------------------------------"
  echo " Downloading HDF5 version ${HDF5_VERSION}"
  echo "-------------------------------------------"
  $DOWNLOAD_PROG  "${HDF5_DOWNLOAD_SITE}/${HDF5_ARCHIVE_NAME}" -o ${HDF5_ARCHIVE_NAME}
fi

if [ ! -e "$SDK_INSTALL/${HDF5_FOLDER_NAME}" ];
then
  tar -xvzf ${HDF5_ARCHIVE_NAME}
# mv hdf5-1.8.15 hdf5-1.8.15_source
fi


# We assume we already have downloaded the source for HDF5 HDF5_VERSION 1.8.18 and have it in a folder
# called hdf5-1.8.18
# SHARED Fortran Libraries are NOT supported on macOS as of HDF 1.8.16
cd ${HDF5_FOLDER_NAME}
mkdir Debug
cd Debug
cmake  -DBUILD_SHARED_LIBS=OFF -DCMAKE_BUILD_TYPE=Debug -DHDF5_BUILD_TOOLS=ON -DHDF5_BUILD_FORTRAN=ON -DHDF5_BUILD_WITH_INSTALL_NAME=ON -DHDF5_BUILD_CPP_LIB=ON -DHDF5_BUILD_HL_LIB=ON -DCMAKE_INSTALL_PREFIX=$SDK_INSTALL/hdf5-${HDF5_VERSION}-Debug ../
make -j${PARALLEL_BUILD}
make install
cd ../
mkdir Release
cd Release
cmake  -DBUILD_SHARED_LIBS=OFF -DCMAKE_BUILD_TYPE=Release -DHDF5_BUILD_TOOLS=ON -DHDF5_BUILD_FORTRAN=ON -DHDF5_BUILD_WITH_INSTALL_NAME=ON -DHDF5_BUILD_CPP_LIB=ON -DHDF5_BUILD_HL_LIB=ON -DCMAKE_INSTALL_PREFIX=$SDK_INSTALL/hdf5-${HDF5_VERSION}-Release ../
make -j${PARALLEL_BUILD}
make install


echo "#--------------------------------------------------------------------------------------------------" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# HDF5 Library" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(HDF5_INSTALL \"\${EMsoft_SDK_ROOT}/hdf5-${HDF5_VERSION}-\${BUILD_TYPE}\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(HDF5_DIR \"\${EMsoft_SDK_ROOT}/hdf5-${HDF5_VERSION}-\${BUILD_TYPE}/share/cmake\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(CMAKE_MODULE_PATH \${CMAKE_MODULE_PATH} \${HDF5_DIR})" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"

