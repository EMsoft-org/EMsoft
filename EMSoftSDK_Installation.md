# EMSoft SDK Setup #

## EMSoft\_SDK Location ##

On all operating systems we like to *sandbox* our installation of the libraries in order to not affect any other software packages on the system. This is optional and can be changed if the user has the knowledge of how to do so. For this example we will locate our **sandbox** at */Users/Shared/EMSoft_SDK* (macOS) or *C:/EMsoft\_SDK* (Windows) or */opt/EMsoft\_SDK* (linux)


## Preliminaries ##

EMSoft relies on the following component libraries:


# Windows 10 #

## SDK Installation Location ##

## Required Minimum Versions of 3rd Party Libraries ##

| Package | Version | Notes |
|---------|---------|-------|
| Intel Fortran | v17 or Greater | Only Tested on Windows 10 |
| JSON Fortran | 4.2.0 | [https://github.com/jacobwilliams/json-fortran](https://github.com/jacobwilliams/json-fortran)|
| HDF5 1.8.18 | 1.8.18 | Must build the Fortran libraries as well as C/C++ libraries |
| CLFortran | CLFortran | [http://dream3d.bluequartz.net/binaries/EMSoft_SDK/EMSoft_SDK/CLFortran_src.zip](http://dream3d.bluequartz.net/binaries/EMSoft_SDK/EMSoft_SDK/CLFortran_src.zip) |
| FFTW | 3.3.4 | Be sure to download the DLL version of FFTW with the proper MSVC version |
| CMake | 3.6.2 | http://www.cmake.org |
| nVidia CUDA | 8.x | [https://developer.nvidia.com/cuda-downloads](https://developer.nvidia.com/cuda-downloads) | 


### Windows 10 with MSVC 2013/2015 & Intel Fortran v17 ###

The current Windows test environment consists of the following elements

+ Windows 10 x64
+ Visual Studio 12 2013 Community or Pro with 64 Bit compilers
+ Intel Fortran v16 or v17
+ CMake 3.5 or 3.6 or 3.7

## HDF5 1.8.18 Compile ##

+ create directory C:/EMsoft_SDK
+ download and install cmake 3. into C:/EMsoft and unzip the cmake archive.
+ download hdf5 1.8.18 from the hdfgroup.org website.
+ copy hdf5-1.8.18.tar.gz into EMsoft_SDK
+ tar -xvzf hdf5-1.8.18.tar.gz
+ Apply patch to allow installation on Windows and FORTRAN in the fortran/src/CMakeLists.txt and gl/fortran/src/CMakeLists.txt files

+ Create hdf5-1.8.18_build/Debug and hdf5-1.8.18_build/Release
+ cd into hdf5-1.8.18_build/Debug
+ run cmake:
	cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=ON -DBUILD_TESTING=OFF -DCMAKE_INSTALL_PREFIX=C:/EMsoft_SDK/hdf5-1.8.18 -DHDF5_BUILD_FORTRAN=ON -DHDF5_ENABLE_F2003=ON ../../hdf5-1.8.18_src
	nmake install
+ cd into hdf5-1.8.18_build/Release
+ run cmake:
	cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DBUILD_TESTING=OFF -DCMAKE_INSTALL_PREFIX=C:/EMsoft_SDK/hdf5-1.8.18 -DHDF5_BUILD_FORTRAN=ON -DHDF5_ENABLE_F2003=ON ../../hdf5-1.8.18_src
	nmake install

### NVidia CUDA/OpenCL Installation ###

Only nVidia GPUs are currently being tested. The developer should download the nVidia CUDA version 8.x developer kit and install that onto their machine. This should occur **after** the installation of Visual Studio but **before** the developer attempts to build the EMsoft SDK components.

+ Download the latest nVidia CUDA SDK (8.x as of Dec 2016) [https://developer.nvidia.com/cuda-downloads](https://developer.nvidia.com/cuda-downloads) and install them.
+ unzip CLFortran_src and cd CLFortran_src
+ Create CLFortran_Build/Debug and CLFortran_Build/Release

		C:\EMsoft_SDK\CLFortran_Build\Debug>cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=C:/EMsoft_SDK/CLFortran ../../CLFortran_src
		nmake install
		cd ../ && mkdir Release && cd Release
		C:\EMsoft_SDK\CLFortran_Build\Debug>cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=C:/EMsoft_SDK/CLFortran ../../CLFortran_src
		nmake install

### Json-Fortran Installation ###

+ Download the json-fortran archive from github version 4.2.0 only. NOT A NEWER VERSION.
+ cd json-fortran
+ mkdir Build && cd Build
		cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=C:/EMsoft_SDK ../
		nmake install

### FFTW Installation ###

+ Download windows DLL version from [http://www.fftw.org/install/windows.html](http://www.fftw.org/install/windows.html)
+ Unzip into C:/EMsoft_SDK/fftw-3.3.34-dll64 directory.
+ run the link.exe program on the fftw DLLs to produce the .lib files:
   	lib /machine:x64 /def:libfftw3-3.def
    lib /machine:x64 /def:libfftw3f-3.def
    lib /machine:x64 /def:libfftw3l-3.def


# Windows 10 #

Only nVidia GPUs are currently being tested. The developer should download the nVidia CUDA version 8.x developer kit and install that onto their machine. This should occur **after** the installation of Visual Studio but **before** the developer attempts to build the EMsoft SDK components.


# macOS #

| Package | Version | Notes |
|---------|---------|-------|
| GFortran | 5.2.0 or Greater | [https://gcc.gnu.org/wiki/GFortranBinaries] (https://gcc.gnu.org/wiki/GFortranBinaries) |
| JSON Fortran | 4.2.0 | [https://github.com/jacobwilliams/json-fortran] (https://github.com/jacobwilliams/json-fortran)|
| HDF5 1.8.18 | 1.8.18 | Must build the Fortran libraries as well as C/C++ libraries |
| CLFortran | CLFortran |  |

## Install FORTRAN Compiler ##

Install gfortran or Intel Fortran on your system. Go to the web site and download/install the appropriate version of gfortran for your OS version.  Make sure that you have at least version 5.2 of GFortran. EMSoft will not compile with earlier gfortran versions.

## OpenCL ##

 OpenCL is included already in the operating system for both the CPU and the GPU. No additional downloads should be necessary

## HDF5 1.8.18 Compile ##

+ create directory /Users/Shared/EMsoft_SDK
+ download and install cmake 3. into C:/EMsoft and unzip the cmake archive.
+ download hdf5 1.8.18 from the hdfgroup.org website.
+ copy hdf5-1.8.18.tar.gz into EMsoft_SDK
+ tar -xvzf hdf5-1.8.18.tar.gz

+ Create hdf5-1.8.18_build/Debug and hdf5-1.8.18_build/Release
+ cd into hdf5-1.8.18_build/Debug
+ run cmake:
	cmake -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=OFF -DBUILD_TESTING=OFF -DCMAKE_INSTALL_PREFIX=/Users/Shared/EMsoft_SDK/hdf5-1.8.18 -DHDF5_BUILD_FORTRAN=ON -DHDF5_ENABLE_F2003=ON ../../hdf5-1.8.18_src
	make install
+ cd into hdf5-1.8.18_build/Release
+ run cmake:
	cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=OFF -DBUILD_TESTING=OFF -DCMAKE_INSTALL_PREFIX=/Users/Shared/EMsoft_SDK/hdf5-1.8.18 -DHDF5_BUILD_FORTRAN=ON -DHDF5_ENABLE_F2003=ON ../../hdf5-1.8.18_src
	make install

### Create EMsoft_SDK.cmake file ###

+ Copy the EMSoft_SDK.cmake file from EMsoftPublic/Support/SDK_Build_Scripts/Windows into the C:/EMsoft_SDK diretory and adjust all the paths to match where everything was built on the current machine.


After the EMsoft SDK is built one needs to create an EMsoft_SDK.cmake file inside the EMsoft_SDK directory. This file will define all the various paths to the 3rd party libraries that are used. Of special consideration for the Intel Fortran compiler the following should be included in the EMsoft_SDK.cmake file

	set(CMAKE_Fortran_FLAGS "/W1 /nologo /fpp /libs:dll /threads /assume:byterecl" CACHE STRING "" FORCE)
	set(CMAKE_EXE_LINKER_FLAGS " /machine:x64 /STACK:100000000" CACHE STRING "" FORCE)

These commands set very **important** compiler flags that are needed to ensure the EMsoft sources codes are compiled in a manner that is expected by the developers. Namely the stack size on Windows is only 1MB by default so we raise it to 100MB. We also tell the Intel Fortran compiler that the record size is 1 byte when reading files, the default is 4 bytes which is NOT the same as GFortran.


## Build SDK ##

There is a shell script in EMSoft/Support/SDK_Build_Scripts/OSX_Build_Scripts/Build_SDK.sh that can be run by the user to download and build the dependent libraries in such a way that EMSoft will compile. For OS X systems the EMSoft_SDK is coded to be in **/Users/Shared/EMSoft_SDK**. If you want this in a different location then the script file can be adjusted as needed. Simply run the script as **sudo** so that the EMSoft_SDK can be created. Note any errors that occur during the process.




# Linux #

| Package | Version | Notes |
|---------|---------|-------|
| GFortran | 5.2.0 or Greater | [https://gcc.gnu.org/wiki/GFortranBinaries] (https://gcc.gnu.org/wiki/GFortranBinaries) |
| JSON Fortran | 4.2.0 | [https://github.com/jacobwilliams/json-fortran] (https://github.com/jacobwilliams/json-fortran)|
| HDF5 1.8.15 | 1.8.15 | Must build the Fortran libraries as well as C/C++ libraries |
| CLFortran | CLFortran |  |


## Build EMSoft (Unix & macOS) ##

We will use CMake to configure a build system for EMSoft. CMake has been included in the EMSoft_SDK for use with EMSoft development. The easiest way to proceed is with a terminal/command prompt on your system of choice. First Navigate to the location of EMSoft.

	[user@system] $ export PATH=$PATH:/Users/Shared/EMSoft_SDK/cmake-3.3.1-Darwin-x86_64/CMake.app/Contents/bin/
	[user@system] $ cd /Path/to/EMSoft
	[user@system] $ mkdir Build
	[user@system] $ cd Build
	[user@system] $ cmake -DEMSoft_SDK=/Users/Shared/EMSoft_SDK -DCMAKE_BUILD_TYPE=Debug ../
	[user@system] $ make -j

After compilation the various programs will be available to execute.

# Linux Installation #

These instructions were developed on Ubuntu 14.04 with an nVidia 650 GTX video card.

## GFortran 5.2 ##

Ubuntu 14.04 comes with GFortran 4.8 which is not new enough for EMSoft to use. We need to install
a newer version (Preferably 5.x, but 4.9.2 should work.). The following commands will get you
the newer GFortran library:

  [user] $ sudo add-apt-repository ppa:ubuntu-toolchain-r/test
  [user] $ sudo apt-get update && sudo apt-get upgrade && sudo apt-get install gfortran-4.9

## OpenCL ##

### Linux ###

Download from (https://software.intel.com/en-us/articles/opencl-drivers)[https://software.intel.com/en-us/articles/opencl-drivers]
The download we are looking for is:
  Intel OpenCL™ Code Builder for Ubuntu (157MB)




