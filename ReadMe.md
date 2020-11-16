# EMsoft Version 5.0

This package contains a series of programs along with a library, mostly written in fortran-90 with some OpenCL bits, for the computation and visualization of scanning electron microscopy diffraction patterns, notably EBSD, ECP, TKD, and EKP. In addition, there are programs for TEM defect image contrast, CBED, PED, Laue x-ray diffraction, and a new series of programs for computational polarized light microscopy. The programs can be used as standalone command-line programs, and produce HDF5 output files that can then be visualized using an IDL virtual app (Interactive Data Language) or read and processed from any other package with HDF5 capability.

This is the first release of version 5.0; this release provides significant additions to the package and many changes under the hood. See below for a detailed list.  We have changed the major version number from 4 to 5 because developers will need to rebuild the EMsoft_SDK before recompiling the code.  This release also coincides with the release of our new spherical indexing code which can be found [here](https://github.com/EMsoft-org/EMSphInx).  Along with the new indexing package comes a new file format for efficient storage of the spherical harmonic transform of EBSD (for now) master patterns; the corresponding C++ code can be found [here](https://github.com/EMsoft-org/SHTfile).

This release also offers, for the first time, a series of python wrappers (*pyEMsoft*) that can be used to access selected *EMsoft* modules from python; an extensive set of examples is provided to illustrate how our library routines can be used. 

Release v5.0.0 has the following DOI (through Zenodo):  

![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3489720.svg)


- ### **This release of EMsoft requires an updated Software Developer Toolkit (SDK); please go to [this page](https://github.com/EMsoft-org/EMsoftSuperbuild) and follow the instructions to install a new SDK before you attempt to build the present release.**

- ### **For Dictionary Indexing (DI) users, we have changed our convention for EBSD pattern orientation to be the same as the vendor convention; i.e., EBSD patterns are displayed looking from the detector to the sample. The practical consequence is that for older DI runs the sign of the xpc (x-component of pattern center) will need to be reversed when used with the Release 5.0 indexing programs.**



## Financial Support 
EBSD/ECP/EKP development of this package, including dictionary indexing for EBSD/ECP, was started with support from an AFOSR/MURI grant, FA9550-12-1-0458; the original EBSD code from CTEMsoft 2.0 was developed with support from an ONR grant, N00014-12-1-0075.  All recent development of EMsoft was performed with support from an ONR Vannevar Bush Faculty Fellowship, N00014-­16-­1-­2821.

## Current and Past Contributors
EMsoft started as a source code base used for the creation of all figures in the *Introduction to Conventional Transmission Electron Microscopy* text book (Cambridge University Press, 2003, ISBN 0521629950) by M. De Graef.  It has since grown into an open source project that has had many contributors and testers over the past 15 years (in no particular order):

- Patrick Callahan
- Saransh Singh
- Stuart Wright
- Elena Pascal
- Will Lenthe
- Chaoyi Zhu
- Joseph Tessmer
- Ke-Wei Jin
- Michael Atkinson
- Joao Fonseca
- Michael Jackson
- Joey Kleingers
- Håkon Wiik Ånes
- McLean Echlin


## Source 
[This site](http://vbff.materials.cmu.edu/EMsoft) conveniently brings all EMsoft stuff together in one place.

For our new spherical indexing code, please go to [this site](http://vbff.materials.cmu.edu/EMSphInx) for all information.

## Installation 

Precompiled binaries (i.e., [nightly builds](http://www.bluequartz.net/binaries/EMsoft/experimental)) requirements:

|  Operating System | Notes |
|-------------------|-------|
| macOS | Version 10.13.6 (High Sierra or newer) |
| Windows 10 | 64 bit, NVidia GPU with latest (3.88) drivers installed |


If you want to build EMsoft yourself, it would make sense to first get a GitHub account, and fork this repository into your account. Then clone the repo *from your account* onto your local computer. Before you can compile things, you need to first build the Software Developer Kit (EMsoft_SDK), which you can find [here](https://github.com/EMsoft-org/EMsoftSuperbuild); follow the instructions for your platform. In addition (as of June 19th, 2019), you will need to clone the *EMsoftData* repository, also from *EMsoft-org*, in a folder at the same level as the Public repository folder. 

Then, starting in the top folder where you have cloned the EMsoft repository, carry out the following commands (for UNIX-type builds; on Windows, use nmake instead of make):

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
Note that *somepath* should be replaced with wherever you installed the SDK.  These commands should compile both a Release and a Debug version of EMsoft. You can then add the path to the EMsoftBuild/Release/Bin folder to your shell path and start using the programs.  Note that the Debug version of the executables will run much more slowly than the Release version, but, if something goes wrong during the run, the error message of the Debug version will be more informative than for the Release version.

To always maintain an up-to-date version of the package, you may want to create a little script that will help you synchronize the repositories and compile in one step.  Here is an example shell script for UNIX-flavored systems; the assumptions are that the EMsoft repository has been cloned into the folder EMsoftPublic, and the EMsoftData repository into EMsoftData:

```fortran
cd EMsoftData
git pull --rebase origin develop
cd ../EMsoftPublic
git pull --rebase origin develop
cd ../EMsoftBuild
make -j

```

If you do not need the complete EMsoft package, you can compile sections of the package (e.g., SEM modalities only) by setting CMake switches using the ccmake GUI program, as described in the ccmake-options.md file. 

## Changes in 5.0.3
This minor update changes the handling of binning in the EMEBSDDI dictionary indexing program; this required additional parameters in the corresponding name list template file.  The changes are described in the EMEBSDDI wiki page.

## Changes in 5.0.2
This update implements simplifications of the HDFsupport module, in particular the argument list for the hyperslab writing and reading routines.  It also provides a modified EMMCOpenCL program with better parameter control for interaction volume runs, as well as a new EMEBSDdefect program (still in further development) for the computation of EBSD patterns when there is a deformation tensor field in the region of interest.

## Changes in 5.0.1
This update makes internal changes to the CMake code that configures the SHT library; the relevant source file is now loaded with the FetchContent function rather than relying on manual git pulls. The EMEBSDmaster program is split into two separate programs, one for standard master patterns, the other (EMEBSDmasterSHT) for the generation of compressed master pattern representations (.sht files) using the spherical harmonic transform.

## New features in 5.0
- There are a few minor bug fixes.
- EMsoft now has two additional ways to manage file paths; please check the descriptions in the *FilePathConventions.md* file.
- We introduce a new diffraction pattern indexing algorithm that we call **EMSphInx**. We have successfully indexed patterns from the following modalities: EBSD, TKD, ECP, transmission Laue. Installing EMSphInx will require that you download the release 0.9 repository from [https:/github.com/EMsoft-org/EMSphInx](https:/github.com/EMsoft-org/EMSphInx) and build it using the CMake configuration scripts (explained on the main EMSphInx page).  *EMSphInx* is distributed under a GPL license, i.e. different from the EMsoft BSD3 license; hence, we keep this set of programs, all written in C++, in a separate repository.
- We make a new file format available for the storage of the spherical harmonic transform of a master pattern; see [https:/github.com/EMsoft-org/SHTfile](https:/github.com/EMsoft-org/SHTfile) for more information. We also make available a database of SHT master patterns for 120 common crystal structures for 5 different microscope accelerating voltages (10, 15, 20, 25, and 30 kV).
- In the *EMSphInx* programs, EBSD pattern binning can handle arbitrary initial and final pattern sizes; the rescaling operation has been implemented using Fourier transforms, so optimal rebinning of a pattern of size 184x184 to one of size 60x60 can now be performed (subject to the usual Nyquist frequency limitations). In addition to the regular  "binning" factors (1x, 2x, 4x etc), the program input files now also accept a rebinned final size whch will override the binning factor. 
- The indexing programs (dictionary indexing and spherical indexing) can now read the binary Oxford .ebsp format (provided that file is not compressed), courtesy of Michael Atkinson (U. Manchester), as well as the NORDIF file format, courtesy of Håkon Wiik Ånes (NTNU, Norway).
- *EMsoft* now uses Intel's Math Kernel Library for the nightly builds.
- We have a new *EMEBSDoverlap* program that allows the user to merge two master patterns with a given orientation relation.  This can be useful to index diffraction patterns from samples for which the scale of the microstructure is such that two phases are intermixed over length scales smaller than the size of the interaction volume.  See the *SEM/EBSD Overlap Master Patterns* wiki for more details.
- In the *EMsampleRFZ* program it is now possible to rotate the entire fundamental zone into an arbitrary orientation; this can be useful in the context of EBSD overlap master patterns or for orientation sampling in non-standard settings.
- We have a simple forward model for Laue diffraction; as for the EBSD case, the algorithm produces a master pattern based on kinematical structure factors.  This can be used with the new spherical indexing algorithm to automatically index large Laue pattern data sets. There is also a separate new *EMLaue* program that can generate series of transmission or reflection Laue patterns.
- We now provide support for Python wrapping! In collaboration with Saransh Singh (LLNL), and with a major effort by Chaoyi Zhu here at CMU, we have restructured our source code to allow for Python routines to call any function/subroutine from a selected subset of EMsoft library modules. For examples, see the pyEMsoft/examples folder inside the Source folder. There is also a new [wiki page]() describing the wrapping process in more detail.
- For those programs for which there is a wiki help page, the user can now generate a PDF version of the help page by providing the -pdf option to the program.  This assumes that the *pandoc* program as well as a LaTeX distribution have been installed and that the program has been added to the search PATH (so far only tested on Mac OS X).
- For TEM diffraction contrast we release a series of new programs that allow the user to simulate STEM-DCI images.  These are essentially 4-D data sets in which there is a 2-D convergent beam electron diffraction (EBSD) pattern for each pixel in a 2-D field of view. The new codes allow for the manual definition of a small number of idealized crystallographic defects (straight dislocations, perfect stacking faults, spherical inclusions and voids), or displacement field input from discrete dislocation dynamics.
- The original Head&Humble code for the simulation of two-beam TEM diffraction contrast images of up to four parallel dislocations with three stacking faults is now available in a more modern form, *EMhh4.f90*, completely integrated with the EMsoft package. This is based on a version of the hh4.f program created in the 1980s by the group of Prof. Skalicky at the University of Vienna.
- There is a simple new program to compute a dictionary of kinematical precession electron diffraction patterns for TEM (*EMPEDkin*).  In its present version, this program does not actually perform any precession computation, but uses a simple kinematical model to compute the diffraction patterns.  A more involved program that does use precession is currently under development. 
- We have a new forward model for polarized light microscopy, based on the Mueller matrix calculus. This includes a model for light scattering from uniaxial crystal structures (e.g., Ti or Zr). Work is underway to incorporate biaxial structures as well as an indexing algorithm to take input images from an optical microscope and return a partial orientation map (c-axis only for uniaxial crystals).

## What's coming in 5.1? 
- We are working on improvements to all underlying libraries.
- Extensive development of the *EMsoftWorkbench* is currently underway at BlueQuartz Software.  We will make the new version, capable of running the dictionary indexing algorithm and refining the EBSD pattern center, available in this release.  
- We hope to make the remainder of the EMsoft modules available in the form of python wrappers, with extensive examples and help files.
- The STEM diffraction contrast image simulations will be able to take input from Molecular Dynamics simulations (LAMMPS).
- For computational polarized light microscopy (CPLM) we will have a new forward model program to predict image series recorded on an optical microscope; we will also release a dictionary indexing program for CPLM.
- The IDL apps have thus far had some issues on Windows and Linux, so we will make available updated apps that are more likely to work correctly.
- We hope to release version 5.1 by the Summer of 2020.

## What's coming in 6.0? 
- We are currently rewriting the entire code base of EMsoft using Object Oriented fortran 2018; this is a major rewrite that completely changes the API, meaning that users who have written their own code to link to or extend EMsoft will likely need to update that code (or continue using the most recent 5.x version).
- All the data types defined in the former typedefs.f90 module are replaced by classes and methods, leading to a significantly cleaner code base.
- We started to use a different package documentation model (FORD for FORtran Documentation) that will have every function, subroutine, class, etc. fully documented in HTML format.
- This is a major recoding effort so for now we won't put a release date on it; it will likely be late 2020 if not Spring of 2021.


## New features in 4.2
- There are a few minor bug fixes
- The main reason for this release is establishing a DOI number so that this version can be linked to a Dictionary Indexing tutorial paper that was published in the journal **Integrating Materials and Manufacturing Innovation** (see top of page).

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


## Licenses ##

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

