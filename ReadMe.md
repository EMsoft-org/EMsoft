# EMsoft Version 4.1

This package contains a series of programs along with a library, mostly written in fortran-90 with some OpenCL bits, for the computation and visualization of scanning electron microscopy diffraction patterns, notably EBSD, ECP, TKD, and EKP. The programs can be used as standalone command-line programs, and produce HDF5 output files that can then be visualized using an IDL virtual app (Interactive Data Language) or read and processed from any other package with HDF5 capability.

This is the first release of version 4.1; once again, there have been substantial changes to how the package is configured, in particular the building of the EMsoft_SDK using the EMsoftSuperbuild repository. 

**You will need to rebuild your SDK using the updated EMsoftSuperbuild!!!**

## Financial Support 
EBSD/ECP/EKP development of this package, including dictionary indexing for EBSD/ECP, ws started with support from an AFOSR/MURI grant, FA9550-12-1-0458; the original EBSD code from CTEMsoft 2.0 was developed with support from an ONR grant, N00014-12-1-0075.  All recent development of TKD and related modalities, including the creation of routines that can generate PoVRay visualization script files, was performed with support from an ONR Vannevar Bush Fellowship, N00014-­16-­1-­2821.

## Source 
[This site](http://vbff.materials.cmu.edu/EMsoft) conveniently brings all EMsoft stuff together in one place.  For nightly builds, please go to [this](http://www.bluequartz.net/binaries/EMsoft/experimental) site and navigate to the most recent date for a compiled version of the entire EMsoft package.  

## Installation 

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


## New features in 4.1
- The EMsoft superbuild has been updated and made more robust, so you will need to rebuild your SDK if you are going to work with this release.
- It has been more than a year since we released version 4.0, so there have been numerous additions and improvements under the hood, plus a number of new programs (see below), too many changes to describe in detail here... if you want to find out everything that's changed, just take a look at the commit messages.
- *EMLACBED* is an updated version from an older version 2.0 program; it can be used to compute a zone axis convergent beam electron diffraction (CBED) pattern using a large convergence angle (e.g., 40 mrad).  The program creates an HDF5 output file with the diffraction disks for a single member of each family of reflections.  You can use the *CBEDDisplay.pro* IDL program to visualize CBED patterns in a couple of different ways. If you do not have an IDL license, try the precompiled [IDL apps](http://vbff.materials.cmu.edu/EMsoft) bullet from where you can download a number of IDL Virtual Machine (VM) apps for several modalities (also new with this release!).
- *EMFitOrientationPS* is a new program in the EBSD program set.  It works similar to the other refinement programs, but it has an option to keep the entire experimental data set in RAM (that leads to somewhat faster execution time); the user can also specify a text file that has pseudo-symmetric variants in it, so that the fit is performed for the best match of the dictionary indexing run, but also for orientations that are related to it by a pseudo-symmetry operation.
- There is a new program *EMEBSDreflectors* that can be used to obtain a ranked reflector list for Hough-based EBSD pattern indexing; the list requires an EBSD master pattern as input and is computed based on the integrated intensity in each Kikuchi band. 
-  We have a new sequence of programs that deal with quasi-crystals, both 2-D (octagonal, decagonal, and dodecagonal) and 3-D (icosahedral).  You can generate a crystal structure file, perform zone axis pattern computations both with a parallel beam and a conical beam (CBED), perform an EBSD master pattern computation, generate a pattern dictionary, and perform dictionary indexing on EBSD patterns from quasi-crystals!  You can turn off compilation of the quasi-crystal related programs by setting the  *EMsoft\_ENABLE\_QC* parameter to OFF using ccmake or as a parameter to cmake.
-  We have updated many of the [wiki help pages](https://github.com/EMsoft-org/EMsoft/wiki) with examples on how to run various programs; this is a work in progress, and we hope to have all programs documented this way by the next release (4.2).
-  We now offer pre-compiled Virtual Machine apps for a number of IDL routines that can be used to visualize some of the EMsoft data files, or carry out EBSD detector parameter fits.  Try the apps out yourself after you download them from [IDL apps](http://vbff.materials.cmu.edu/EMsoft). 
-  We have added several C-callable routines to the *EMsoftWrapperLib* folders; these are versions of other library functions that have been made callable by C and C++ programs.  Documentation for all routines is underway and will be posted on the previously mentioned wiki pages. 
- Unfortunately, work on the *EMsoftWorkbench* has come to a temporary stand-still due to lack of development funds. Hopefully we will be able to pick that thread back up at some point in the near future...

## What's coming in 4.2? 
- We are working on improvements to all underlying libraries.
- We will have a new Monte Carlo program using the Discrete Losses Approximation (DLA) instead of the Continuous Slowing Down Approximation (CSDA).  DLA produces a better result than CSDA, in particular with respect to the zero-loss peak (which is pretty much absent with CSDA).  So, in principle, we should get even better agreement with experimental EBSD, ECP, and TKD patterns!
- In July of 2018, we started working on a new pattern indexing technique that we call *SphInx*; this approach relies on a spherical fast Fourier transform of an EBSD, ECP, or TKD master pattern to index experimental patterns.  This will be significantly faster than the current version of dictionary indexing and we are really excited about this new approach!
- On the TEM side, we will have a number of new programs for the computation of STEM-DCI (diffraction contrast) images, using displacement field input from phase field, molecular dynamics, and discrete dislocation dynamics simulations.  We will also extend this approach to the SEM-based ECCI modality, and introduce the capability to compute EBSD patterns for deformed materials (i.e., containing dislocation networks derived from discrete dislocation dynamics computations).
- With financial support from the Naval Research Lab, BlueQuartz can continue work on the *EMsoftWorkbench*; expect a new and more powerful version in this release.
- We hope to add a few programs for the computation of optical polarized light intensity curves based on a Mueller-matrix approach.
- Some of our developers told us they have been working on python wrappers for *EMsoft* !!!  If they are willing to make these available to us, then we will make sure they become part of one of the next releases.
- We hope to release version 4.2 sometime in the late Spring of 2019.


## License ##

	!###################################################################
	! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
	! All rights reserved.
	!
	! Redistribution and use in source and binary forms, with or without modification, are 
	! permitted provided that the following conditions are met:
	!
	!     - Redistributions of source code must retain the above copyright notice, this list 
	!        of conditions and the following disclaimer.
	!     - Redistributions in binary form must reproduce the above copyright notice, this 
	!        list of conditions and the following disclaimer in the documentation and/or 
	!        other materials provided with the distribution.
	!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
	!        of its contributors may be used to endorse or promote products derived from 
	!        this software without specific prior written permission.
	!
	! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
	! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
	! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
	! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
	! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
	! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
	! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
	! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
	! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
	! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
	! ###################################################################

