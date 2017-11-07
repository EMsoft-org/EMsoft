#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "This script requires 2 arguments: Path where you want the SDK Installed and the "
    echo "the number of build threads to use when building. For example if you pass "
    echo "'Build_SDK.sh /opt/EMsoft_SDK 8' then /opt/EMsoft_SDK will be the folder"
    echo "that has all the dependent library folders in it."
    exit 
fi

SDK_INSTALL=${1}
PARALLEL_BUILD=${2}

echo "SDK_INSTALL=$SDK_INSTALL"
echo "PARALLEL_BUILD=$PARALLEL_BUILD"


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

if [[ -e /usr/bin/gfortran-5 ]]; then
  export FC=/usr/bin/gfortran-5
fi

cd $SDK_INSTALL

HOST_SYSTEM=`uname`
echo "Host System: $HOST_SYSTEM"

if [ ! -e "$SDK_INSTALL/${FFTW_FOLDER_NAME}-${FFTW_VERSION}.tar.gz" ];
then
  echo "-------------------------------------------"
  echo " Downloading fftw Version ${FFTW_VERSION}"
  echo "-------------------------------------------"
  $DOWNLOAD_PROG  "http://www.fftw.org/fftw-${FFTW_VERSION}.tar.gz" -o ${FFTW_FOLDER_NAME}-${FFTW_VERSION}.tar.gz
fi

if [ ! -e "$SDK_INSTALL/${FFTW_FOLDER_NAME}-${FFTW_VERSION}" ];
then
  tar -xvzf ${FFTW_FOLDER_NAME}-${FFTW_VERSION}.tar.gz
fi

# We assume we already have downloaded the source for fftw3 and have it in a folder
# called fftw-3.3.4
cd ${FFTW_FOLDER_NAME}-${FFTW_VERSION}

# ./configure --prefix=$SDK_INSTALL/fftw  --enable-shared --enable-threads --enable-openmp 
./configure --prefix=$SDK_INSTALL/${FFTW_FOLDER_NAME} --enable-shared 
make -j${PARALLEL_BUILD}
make install

#------------------------------------------------------------------------------
# This next bit of code sets the install name of the dylib to the full absolute
# path of the library. This will come in handy when packagin EMSoft with CMake
# by allowing CMake to more easily find the library and adjust its internal paths

echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "#--------------------------------------------------------------------------------------------------" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# FFTW3 Library" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(FFTW3_INSTALL \"\${EMsoft_SDK_ROOT}/${FFTW_FOLDER_NAME}\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(FFTW3_VERSION \"${FFTW_VERSION}\")" >>  "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"

