# EMsoft Version 4.3

This package contains a series of programs along with a library, mostly written in fortran-90 with some OpenCL bits, for the computation and visualization of scanning electron microscopy diffraction patterns, notably EBSD, ECP, TKD, and EKP. The programs can be used as standalone command-line programs, and produce HDF5 output files that can then be visualized using an IDL virtual app (Interactive Data Language) or read and processed from any other package with HDF5 capability.

This is the first release of version 4.3; this release provides significant additions to the package and many changes under the hood. See below for a detailed list.  There is also an important change to the open source model: a portion of our source code is now licensed for commercial entities.  Academic and government users can continue to use the source under the BSD2 license, as before.

The Dictionary Indexing tutorial paper has now been published in the journal **Integrating Materials and Manufacturing Innovation**. The EMsoft version that goes along with this paper is version 4.2; this release has the following DOI (through Zenodo):  

![Zenodo DOI Badge](https://zenodo.org/badge/109896059.svg)

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

## New features in 4.3
- There are a few minor bug fixes
- We now provide support for Python wrapping! Thanks to Saransh Singh (LLNL), we have slightly restructured our source code to allow for Python routines to call any of the EMsoft library routines. For an example, see XXX. There is also a new wiki page describing this wrapping process in more detail.
- We introduce a new diffraction pattern indexing algorithm that we call **EMSphInx**. We have successfully indexed patterns from the following modalities: EBSD, TKD, ECP, transmission Laue. 
- For TEM diffraction contrast we release a series of new programs that allow the user to simulate STEM-DCI images.  These are essentially 4-D data sets in which there is a 2-D convergent beam electron diffraction (EBSD) pattern for each pixel in a 2-D field of view. The new codes allow for the manual definition of a small number of idealized crystallographic defects (straight dislocations, perfect stacking faults, spherical inclusions and voids), or displacement field input from phase field computations (currently only for gamma-gamma' superalloy microstructures), discrete dislocation dynamics, or molecular dynamics.
- EMsoft now has two additional ways to manage file paths; please check the descriptions in the *FilePathConventions.md* file.
- We have a new forward model for polarized light microscopy, based on the Mueller matrix calculus. This includes a model for light scattering from uniaxial crystal structures (e.g., Ti). Work is underway to incorporate biaxial structures as well as an indexing algorithm to take input images from an optical microscope and return a partial orientation map (c-axis only for uniaxial crystals).
- We have a simple forward model for Laue diffraction; as for the EBSD case, the algorithm produces a master pattern based on kinematical structure factors.  This can be used to generate transmission Laue patterns (reflection Laue will be added later); the user can also apply the new spherical indexing algorithm to automatically index large Laue pattern data sets.


## New features in 4.2
- There are a few minor bug fixes
- The main reason for this release is establishing a DOI number so that this version can be linked to a Dictionary Indexing tutorial paper that will be published in the journal **Integrating Materials and Manufacturing Innovation**

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

## What's coming in 4.4? 
- We are working on improvements to all underlying libraries.
- We hope to release version 4.4 by the end of 2019.


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

