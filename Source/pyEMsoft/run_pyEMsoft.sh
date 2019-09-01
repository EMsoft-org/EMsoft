#!/bin/bash
###################################################################
# Copyright (c) 2014-2019, Marc De Graef Research Group/Carnegie Mellon University
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification, are 
# permitted provided that the following conditions are met:
#
#     - Redistributions of source code must retain the above copyright notice, this list 
#        of conditions and the following disclaimer.
#     - Redistributions in binary form must reproduce the above copyright notice, this 
#        list of conditions and the following disclaimer in the documentation and/or 
#        other materials provided with the distribution.
#     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
#        of its contributors may be used to endorse or promote products derived from 
#        this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
# USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# ###################################################################
#--------------------------------------------------------------------------
# EMsoft:run_pyEMsoft.sh
#--------------------------------------------------------------------------
#
# PROGRAM: run_pyEMsoft.sh
#
#> @author Chayoi Zhu, Marc De Graef
# 
#> @note: bash script to generate python wrappers for EMsoft
#
#> @date 08/30/10 MDG 1.0 original 
#--------------------------------------------------------------------------

#=======================
#=======================
# this script assumes a properly functioning anaconda 3 environment 
# along with a correctly installed f90wrap package
#=======================
#=======================

# declare the arrays of source files that need to be included in this python wrapper build;
# these files are listed in the order that they are make'd in a regular EMsoft build.
declare -a f90_source_files=("io.f90"
                             "error.f90" 
                             "CLsupport.f90" 
                             "constants.f90" 
                             "math.f90"
                             "typedefs.f90"
                             "crystal.f90" 
                             "symmetry.f90" 
                             "files.f90" 
                             "others.f90"
                             "quaternions.f90" 
                             "Lambert.f90"
                             "rotations.f90") 

declare -a f90_HDF_source_files=("HDFsupport.f90")

declare -a f90_generated_source_files=("stringconstants.f90"
                                       "local.f90")

#=======================
# no changes should need to be made below this line
#=======================

#=======================
# the following folders can probably be set via CMake variables
EMsoft_BUILDfolder=/Users/mdg/Files/EMsoftBuild
EMsoft_folder=/Users/mdg/Files/EMsoftPublic
CondaLib=/Users/mdg/anaconda3/lib
EMsoft_SDK=/Users/Shared/EMsoft_SDK

#=======================
# derived folders
EMsoftLib=${EMsoft_folder}/Source/EMsoftLib
EMsoftHDFLib=${EMsoft_folder}/Source/EMsoftHDFLib
EMsoftBuildLib=${EMsoft_BUILDfolder}/EMsoft/EMsoftLib

#=======================
# clean up from any previous builds (if they exist)
# [this form of test makes sure that no unnecessary messages
# are printed due to files not existing]
echo " run_pyEMsoft.sh: Cleaning up from previous wrapper builds"
if ls source/__pycache__ 1> /dev/null 2>&1; then
    rm -rf source/__pycache__
fi

if ls source/*.so 1> /dev/null 2>&1; then
	rm -rf source/*.so 
fi

if ls source/pyEMsoft.py 1> /dev/null 2>&1; then
	rm -rf source/pyEMsoft.py 
fi

if ls source/*.f90 1> /dev/null 2>&1; then
	rm -rf source/*.f90
fi

if ls *.f90 1> /dev/null 2>&1; then
    rm -rf *.f90 
fi

#=======================
# copy all relevant source files to the present folder 
echo " run_pyEMsoft.sh: Copying source files into place"
for file in "${f90_source_files[@]}"
do
	cp ${EMsoftLib}/${file} .
done

for file in "${f90_HDF_source_files[@]}"
do
	cp ${EMsoftHDFLib}/${file} .
done

for file in "${f90_generated_source_files[@]}"
do
	cp ${EMsoftBuildLib}/${file} .
done 

#=======================
# execute the f90wrap program using all the files just copied
echo " run_pyEMsoft.sh: executing f90wrap"
#f90wrap -k kind_map -m pyEMsoft  ${f90_generated_source_files[*]} ${f90_source_files[*]} 1>build.log 2>build_error.log # ${f90_HDF_source_files[*]}
f90wrap -k kind_map -m pyEMsoft  ${f90_generated_source_files[*]} ${f90_source_files[*]} # ${f90_HDF_source_files[*]}

#=======================
# call f2py-f90wrap to build the wrapper library
echo " run_pyEMsoft.sh: executing f2py-f90wrap"
f2py-f90wrap -c -m _pyEMsoft f90wrap_*.f90 -I$EMsoft_BUILDfolder/EMsoft/EMsoftLib \
-I$EMsoft_BUILDfolder/EMsoft/EMsoftHDFLib \
-I$EMsoft_SDK/hdf5-1.8.20-Release/include/static \
-I$EMsoft_SDK/CLFortran-0.0.1-Release/include \
-I$EMsoft_SDK/jsonfortran-4.2.1-Release/include \
-I$EMsoft_SDK/fftw-3.3.8/include \
-L$EMsoft_BUILDfolder/Bin \
-L$EMsoft_SDK/jsonfortran-4.2.1-Release/lib \
-L$EMsoft_SDK/CLFortran-0.0.1-Release/lib \
-L$EMsoft_SDK/hdf5-1.8.20-Release/lib \
-L$EMsoft_SDK/fftw-3.3.8/lib \
-L$CondaLib \
-lblas -llapack -lEMsoftLib -ljsonfortran -lhdf5 -lclfortran -lfftw3 # 1>>build.log 2>>build_error.log

#=======================
# move f90wrap files to sources folder and clean up
echo " run_pyEMsoft.sh: moving files into proper locations"
mkdir source 
mv f90wrap_*.f90 source
mv pyEMsoft.py source 
mv *.so source 

#=======================
# clean up all the other .f90 files since we no longer need them 
echo " run_pyEMsoft.sh: cleaning up"
rm *.f90 

# that's it
