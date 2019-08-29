#!/bin/sh

EMsoft_BUILDfolder=/Users/mdg/Files/EMsoftBuild
EMsoft_folder=/Users/mdg/Files/EMsoftPublic
CondaLib=/Users/mdg/anaconda3/lib
EMsoft_SDK=/Users/Shared/EMsoft_SDK
EMsoftLib=${EMsoft_folder}/Source/EMsoftLib
EMsoftHDFLib=${EMsoft_folder}/Source/EMsoftHDFLib
EMsoftBuildLib=${EMsoft_BUILDfolder}/EMsoft/EMsoftLib

rm -rf *.so pyEMsoft.py

f90_source_files[0]=typedefs.f90 
f90_source_files[1]=crystal.f90 
f90_source_files[2]=constants.f90
f90_source_files[3]=math.f90 
f90_source_files[4]=symmetry.f90 
f90_source_files[5]=quaternions.f90 
f90_source_files[6]=rotations.f90
f90_source_files[7]=io.f90 
f90_source_files[8]=files.f90 
f90_source_files[9]=error.f90 
f90_source_files[10]=Lambert.f90
f90_HDF_source_files[0]=HDFsupport.f90 
f90_generated_source_files[0]=local.f90 
f90_generated_source_files[1]=stringconstants.f90

for i in 0 1 2 3 4 5 6 7 8 9 10
do
	cp ${EMsoftLib}/${f90_source_files[i]} .
done

cp ${EMsoftHDFLib}/${f90_HDF_source_files[0]} .

for i in 0 1
do
	cp ${EMsoftBuildLib}/${f90_generated_source_files[i]} .
done 

f90wrap -k kind_map -m pyEMsoft ${f90_source_files[*]} ${f90_generated_source_files[*]} # ${f90_HDF_source_files[*]}

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
-lblas -llapack -lEMsoftLib -ljsonfortran -lhdf5 -lclfortran -lfftw3

# move f90wrap files to sources folder and clean up
mkdir sources 
mv f90wrap_*.f90 sources
rm *.f90 

