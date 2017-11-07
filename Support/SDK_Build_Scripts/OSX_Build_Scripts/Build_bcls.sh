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
HOST_SYSTEM=`uname`
echo "SDK_INSTALL=$SDK_INSTALL"
echo "PARALLEL_BUILD=$PARALLEL_BUILD"
echo "Host System: $HOST_SYSTEM"


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

if [ ! -e "$SDK_INSTALL/${bcls_ARCHIVE_NAME}-${bcls_VERSION}.tar.gz" ];
then
  echo "-------------------------------------------"
  echo " Downloading bcls Version ${bcls_VERSION}"
  echo "-------------------------------------------"
  git clone -b $bcls_GIT_BRANCH git://$bcls_GIT_REPO $bcls_FOLDER_NAME
fi


# We assume we already have downloaded the source for FortranCL and have it extracted
cd $SDK_INSTALL/${bcls_FOLDER_NAME}

mkdir Build
cd Build
$SDK_INSTALL/$CMAKE_FOLDER_NAME/${CMAKE_EXE_PATH}/bin/cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=$SDK_INSTALL/${bcls_INSTALL_NAME}-${bcls_VERSION}-Debug ../
make -j$PARALLEL_BUILD
make install

cd ../
mkdir zRel
cd zRel
$SDK_INSTALL/$CMAKE_FOLDER_NAME/${CMAKE_EXE_PATH}/bin/cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$SDK_INSTALL/${bcls_INSTALL_NAME}-${bcls_VERSION}-Release ../
make -j$PARALLEL_BUILD
make install




echo "#--------------------------------------------------------------------------------------------------" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "# bcls  Library" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "set(bcls_DIR \"\${EMsoft_SDK_ROOT}/${bcls_INSTALL_NAME}-${bcls_VERSION}-\${BUILD_TYPE}/lib/cmake/bcls\")" >> "$SDK_INSTALL/EMsoft_SDK.cmake"
echo "" >> "$SDK_INSTALL/EMsoft_SDK.cmake"



