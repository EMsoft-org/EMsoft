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

if [ ! -e "$SDK_INSTALL/${CLFORTRAN_ARCHIVE_NAME}-${CLFORTRAN_VERSION}.tar.gz" ];
then
  echo "-------------------------------------------"
  echo " Downloading CLFORTRAN Version ${CLFORTRAN_VERSION}"
  echo "-------------------------------------------"
  git clone -b $CLFORTRAN_GIT_BRANCH git://$CLFORTRAN_GIT_REPO $CLFORTRAN_FOLDER_NAME
fi


# We assume we already have downloaded the source for FortranCL and have it extracted
cd $SDK_INSTALL/${CLFORTRAN_FOLDER_NAME}

mkdir Build
cd Build
cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=$SDK_INSTALL/$CLFORTRAN_INSTALL_NAME-Debug ../
make -j$PARALLEL_BUILD
make install

cd ../
mkdir zRel
cd zRel
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$SDK_INSTALL/$CLFORTRAN_INSTALL_NAME-Release ../
make -j$PARALLEL_BUILD
make install




echo "#--------------------------------------------------------------------------------------------------" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# CLFORTRAN  Library" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(CLFORTRAN_INSTALL \"\${EMsoft_SDK_ROOT}/${CLFORTRAN_INSTALL_NAME}-\${BUILD_TYPE}\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(CLFORTRAN_VERSION \"\${CLFORTRAN_VERSION}\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"


