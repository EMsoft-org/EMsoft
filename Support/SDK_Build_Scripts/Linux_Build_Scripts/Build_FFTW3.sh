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
#export FC=/usr/bin/gfortran-5
cd $SDK_INSTALL

HOST_SYSTEM=`uname`
echo "Host System: $HOST_SYSTEM"

if [ ! -e "$SDK_INSTALL/${FFTW_FOLDER_NAME}-${FFTW_VERSION}.tar.gz" ];
then
  echo "-------------------------------------------"
  echo " Downloading fftw Version ${FFTW_VERSION}"
  echo "-------------------------------------------"
  $DOWNLOAD_PROG  "" -o ${FFTW_FOLDER_NAME}-${FFTW_VERSION}.tar.gz
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
#cd ${SDK_INSTALL}/${FFTW_FOLDER_NAME}/lib
#install_name_tool -id ${SDK_INSTALL}/${FFTW_FOLDER_NAME}/lib/libfftw3.3.dylib ${SDK_INSTALL}/${FFTW_FOLDER_NAME}/lib/libfftw3.3.dylib 
# install_name_tool -id ${SDK_INSTALL}/${FFTW_FOLDER_NAME}/lib/libfftw3_threads.3.dylib ${SDK_INSTALL}/${FFTW_FOLDER_NAME}/lib/libfftw3_threads.3.dylib 
# install_name_tool -id ${SDK_INSTALL}/${FFTW_FOLDER_NAME}/lib/libfftw3_omp.3.dylib ${SDK_INSTALL}/${FFTW_FOLDER_NAME}/lib/libfftw3_omp.3.dylib 

echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "#--------------------------------------------------------------------------------------------------" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# FFTW3 Library" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(FFTW3_INSTALL \"\${EMsoft_SDK_ROOT}/${FFTW_FOLDER_NAME}\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(FFTW3_VERSION \"${FFTW_VERSION}\")" >>  "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"

