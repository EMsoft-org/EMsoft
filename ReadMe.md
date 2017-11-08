# EMsoft Version 4.0 #

This package contains a series of programs along with a library, mostly written in fortran-90 with some OpenCL bits, for the computation and visualization of scanning electron microscopy diffraction patterns, notably EBSD, ECP, TKD, and EKP. The programs can be used as standalone command-line programs, and produce HDF5 output files that can then be visualized using an IDL virtual app (Interactive Data Language) or read and processed from any other package with HDF5 capability.

This is the first release of version 4.0; there have been substantial changes to how the package is configured, in particular the building of the EMsoft_SDK using the EMsoftSuperbuild repository.

## Financial Support ##
Current EBSD/ECP/EKP development of this package, including dictionary indexing for EBSD/ECP, is supported by an AFOSR/MURI grant, FA9550-12-1-0458; the original EBSD code from CTEMsoft 2.0 was developed with support from an ONR grant, N00014-12-1-0075.  More recent development of TKD and related modalities, including the creation of routines that can generate PoVRay visualization script files, was performed with support from an ONR Vannevar Bush Fellowship, N00014-­16-­1-­2821.

## Source ##
Source codes are here on GitHub. Some precompiled OS X binaries, manuals, and IDL Virtual Machine apps, as well as additional information about the package are located at [Here](http://muri.materials.cmu.edu/?p=858).  For nightly builds, please go to [this](http://www.bluequartz.net/binaries/EMsoft/experimental) site and navigate to the most recent date for a compiled version of the entire EMsoft package.

## Installation ##

Precompiled binaries requirements:

|  Operating System | Notes |
|-------------------|-------|
| macOS | Version 10.12.6 (Sierra or newer) |
| Windows 10 | 64 bit, NVidia GPU with latest (3.88) drivers installed |

## New features in 4.0 ##
- *EMMCfoil*, *EMTKDmaster*, *EMTKD*, *EMTKDDI*: this is a sequence of programs to perform a Monte Carlo simulation for the TKD geometry, then compute a master pattern (same conventions as for EBSD), simulate individual patterns, and finally perform dictionary indexing for experimental TKD patterns.  This is the first release of these programs, so we welcome any feedback and issue reports.
- *EMFitOrientation* and *EMRefineOrientation*: these are two new programs in the EBSD program set.  After you have performed a dictionary indexing run, the best matching orientation is one of the orientations belonging to the cubochoric grid of points used to generate the dictionary. With the *EMRefineOrientation*, you can refine each indexed orientation by means of a cubochoric grid surrounding the best matching grid point of the original indexing run, and then gradually reduce the size of the grid to find a better matching orientation.  Alternatively, you can use the *EMFitOrientation* program to perform a derivative-free optimization of the orientation; this works a little faster than the *EMRefineOrientation* program.  Either program can be used to get rid of artifacts caused by the discrete nature of the orientation grid used for dictionary indexing.
- The *EMsoftWorkbench* has been significantly improved, thanks to our friends at BlueQuartz Software. The workbench should function properly on both Mac OS X and Windows platforms.  The *EMsoftWorkbench* (along with all the other programs) is also available in pre-built form from the BlueQuartz web site (see link above); please keep in mind that those builds are nightly builds from the most recent source code, so on occasion those builds might fail.

## License ##

	!###################################################################
	! Copyright (c) 2013-2017, Marc De Graef Research Group/Carnegie Mellon University
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

