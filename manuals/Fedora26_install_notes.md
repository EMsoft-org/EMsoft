<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Installing EMsoft on a fresh Fedora (25/26) install</a>
<ul>
<li><a href="#sec-1-1">1.1. install dkms</a></li>
<li><a href="#sec-1-2">1.2. Install opencl software</a></li>
<li><a href="#sec-1-3">1.3. Install cmake3</a></li>
<li><a href="#sec-1-4">1.4. install gcc-c++</a></li>
<li><a href="#sec-1-5">1.5. Install gcc-gfortran</a></li>
<li><a href="#sec-1-6">1.6. Install LAPACK</a></li>
<li><a href="#sec-1-7">1.7. Procedure for installing SDK</a></li>
<li><a href="#sec-1-8">1.8. Procedure for building EMsoft</a></li>
<li><a href="#sec-1-9">1.9. EMsoft configuration</a></li>
<li><a href="#sec-1-10">1.10. IDL routines</a>
<ul>
<li><a href="#sec-1-10-1">1.10.1. Install IDL</a></li>
<li><a href="#sec-1-10-2">1.10.2. Fix links to EMsoftLib library</a></li>
<li><a href="#sec-1-10-3">1.10.3. Change IDL installation scripts</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>


# Installing EMsoft on a fresh Fedora (25/26) install<a id="sec-1" name="sec-1"></a>

## install dkms<a id="sec-1-1" name="sec-1-1"></a>

    >>sudo dnf install dkms

## Install opencl software<a id="sec-1-2" name="sec-1-2"></a>

The Intel OpenCl software can be downloaded from here:
<https://software.intel.com/en-us/articles/opencl-drivers>
You will need at least the runtime drivers. Installing the SDK DOES NOT install the runtime drivers.

The NVIDIA driver is usually installed when the graphics card is installed. Installing an NVIDIA card on Fedora has usually bee painful, however, there is a nice repository with a nice, well structured version of the drivers which works well with Fedora 25/26:
<https://negativo17.org/nvidia-driver/>
you will need to install the CUDA tools and libraries (nvidia-cuda-devel) for it to work.

## Install cmake3<a id="sec-1-3" name="sec-1-3"></a>

    >>sudo dnf install cmake3

## install gcc-c++<a id="sec-1-4" name="sec-1-4"></a>

    >>sudo dnf install gcc-c++

## Install gcc-gfortran<a id="sec-1-5" name="sec-1-5"></a>

    >>sudo dnf install gcc-fortran

## Install LAPACK<a id="sec-1-6" name="sec-1-6"></a>

    >>sudo dnf install lapack-devel

## Procedure for installing SDK<a id="sec-1-7" name="sec-1-7"></a>

Edit directory name in SDK\_conf to indicate where the SDK should be installed. In this example the directory is (here **/home/user/EMsoftSDK**)

Run linux script in: 

***EMsoftPublic/Support/SDK\_Build\_Scripts/Linux\_Build\_Scripts***

The main script, Build\_SDK.sh, downloads the latest versions of the software needed to build EMsoft and compiles them. This will only work properly if the dependencies above (cmake, lapack-devel etc.) have all been installed.

Once the SDK has been installed properly we can build EMsoft.

## Procedure for building EMsoft<a id="sec-1-8" name="sec-1-8"></a>

In the EMsoftPublic directory, create a Build directory:

    >>mkdir Build

Then change to the Build directory 

    >>cd Build

and run cmake. If you have installed cmake3 this can be done with the following command: 

    >>cmake -DEMsoft_SDK=/home/user/EMsoft_SDK -DCMAKE_BUILD_TYPE=Debug ../

note that there is a typo in the other instructions file (it has EMSoft\_SDK instead of EMsoft\_SDK). If cmake complains that it hasn't found something, this is because either the SDK wasn't installed cleanly or the path to the SDK, which is passed on with "-DEMsoft\_SDK =/home/user/EMsoft\_SD, has not been passed on properly. There should never be a need to edit the configuation files cmake creates.

Finally run make to compile EMsoft:

    >>make

## EMsoft configuration<a id="sec-1-9" name="sec-1-9"></a>

The first thing to do is run EMsoftinit in **Bin/** directory and follow the instructions to define the different paths. This includes defining the path for the EMsoft folder, the EMsoft data file and the path for the libraries for the IDL routines (***home/user/EMsoftPublic/Build/Bin***).   

## IDL routines<a id="sec-1-10" name="sec-1-10"></a>

### Install IDL<a id="sec-1-10-1" name="sec-1-10-1"></a>

Firstly you will need an IDL licence (and installation) so that you can install and compile the IDL routines. How you will do this will depend on your local arrangements.

### Fix links to EMsoftLib library<a id="sec-1-10-2" name="sec-1-10-2"></a>

The next thing to do is to create **.dylib** versions of the **.so** shared libraries. The files are identical but the IDL programs look for a **.dylib** file whereas the compiler produces the linux **.so** file. Alternatively, one can replace the calls to libEMsoftLib.dylib with libEMsoftLib.so in the **.pro** files in the **EMsoftPublic/IDL/pro** directory:

    >>sed -i -- 's/dylib/so/g' *

The dylib version is a dynamic library, whereas the default compilation settings for EMsoft generate static libraries.  You can generate the dynamic libraries by rebuilding EMsoft with the cmake option -DBUILD_SHARED_LIBS=ON.

### Change IDL installation scripts<a id="sec-1-10-3" name="sec-1-10-3"></a>

There are 3 installation scripts for creating virtual IDL machines that can then be run without a license. These are mkVM\_Efitgui.txt, mkVM\_SEMgui.txt and mkVM\_ellipsometrytool.txt. These need to be edited to change the directory information to reflect your local configuration and to change the compilation flag from MACINT64 to LIN64. After this, going into the IDL prompt and entering @mkVM\_Efitgui.txt should create a virtual machine in the directory defined in the \*.txt script. In an earlier version, this produced a bunch of compilation errors, but those were eliminated by fixing the capitalization and changing the order in which routines were compiled with the @ command.  **[Thanks JQFonseca for figuring out the necessary fixes]**

