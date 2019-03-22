# EMSphInx

*EMSphInx* is a collection of modules and programs that can be used to index a variety of diffraction patterns, ranging from EBSD to TKD, ECP, and x-ray Laue patterns.  The source code is available in two languages, C++ and f90.  The C++ version can be compiled as a stand-alone package, the f90 version must be built as part of the *EMsoft* package; build instructions can be found below.  Since the code is considered to be part of *EMsoft*, the versioning number for *EMsoft* will be inherited by all *EMSphInx* code.

### It should be noted that *EMSphInx* has a *non-commercial research use* license, different from the rest of *EMsoft* which is available under a BSD2 license.  The user is encouraged to read the **RAUSSlicense.txt** file that contains the detailed Software License Agreement.  This license applies to all the files contained within the EMSphInx folder.




## Financial Support 
The *EMSphInx* code was developed with support from an ONR Vannevar Bush Faculty Fellowship grant, N00014-­16-­1-­2821.



## Build Instructions 

Precompiled binaries (i.e., [nightly builds](http://www.bluequartz.net/binaries/EMsoft/experimental)) requirements:

|  Operating System | Notes |
|-------------------|-------|
| macOS | Version 10.12.6 (Sierra or newer) |
| Windows 10 | 64 bit, NVidia GPU with latest (3.88) drivers installed |


If you want to build EMsoft yourself, it would make sense to first get a GitHub account, and fork this repository into your account. Then clone the repo *from your account* onto your local computer. Before you can compile things, you need to first build the Software Developer Kit (EMsoft_SDK), which you can find [here](https://github.com/EMsoft-org/EMsoftSuperbuild); follow the instructions for your platform. Then, in the top folder where you have cloned the EMsoft repository, carry out the following commands (for UNIX-type builds; on Windows, use nmake instead of make):

```fortran
  mkdir EMsoftBuild
  cd EMsoftBuild
  mkdir Release
  cd Release
  cmake -DCMAKE_BUILD_TYPE=Release -DEMsoft_SDK=/somepath/EMsoft_SDK ../../EMsoft
  make -j
  cd ../
  mkdir Debug
  cd Debug
  cmake -DCMAKE_BUILD_TYPE=Debug -DEMsoft_SDK=/somepath/EMsoft_SDK ../../EMsoft
  make -j

```
Note that *somepath* should be replaced with wherever you installed the SDK.  These commands should compile both a Release and a Debug version of EMsoft. You can then add the path to the EMsoftBuild/Release/Bin folder to your shell path and start using the programs.  Note that the Debug version of the executables will run much more slowly than the Release version.

If you do not need the complete EMsoft package, you can compile sections of the package (e.g., SEM modalities only) by setting CMake switches using the ccmake GUI program, as described in the ccmake-options.md file. 

## New features in 4.2
- There are a few minor bug fixes
- The main reason for this release is establishing a DOI number so that this version can be linked to a Dictionary Indexing tutorial paper that will be published in the journal **Integrating Materials and Manufacturing Innovation**


## What's coming in 4.3? 
- We are working on improvements to all underlying libraries.
- We hope to release version 4.3 sometime in the late Spring/early Summer of 2019.


## License ##
