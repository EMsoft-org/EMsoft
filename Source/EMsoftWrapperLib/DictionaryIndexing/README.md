# PyEMEBSDDI
**PyEMEBSDDI** is a package under [**EMsoft**](https://github.com/EMsoft-org/EMsoft), providing basic Python wrappers for Dictionary Indexing (DI) functions. The objective is to allow users to apply dictionary indexing and refinement conveniently inside Python.

We also develop a higher-level wrapper for **PyEMEBSDDI**, called [**PyEMEBSDDI_wrapper**](https://github.com/Darkhunter9/PyEMEBSDDI_wrapper). It has a more intuitive method to pass all parameters, high efficient multi-GPU support, and process EBSD patterns. This higher-level wrapper is more user-friendly and can be easily install via [pip](https://pypi.org/project/PyEMEBSDDI-wrapper/).


## Features
1. Written in C/C++ and Fortran, high operational efficiency;
2. Main the same arguments as original **EMsoft** functions;


## Source
1. [**EMsoft**](https://github.com/EMsoft-org/EMsoft) github page;
2. [**PyEMEBSDDI**](https://github.com/EMsoft-org/EMsoft/tree/develop/Source/EMsoftWrapperLib/DictionaryIndexing) github page (under **EMsoft**);
3. [**PyEMEBSDDI_wrapper**](https://github.com/Darkhunter9/PyEMEBSDDI_wrapper) (higher-level Python wrappers for PyEMEBSDDI) github page;
4. [**PyEMEBSDDI_wrapper**](https://pypi.org/project/PyEMEBSDDI-wrapper/) PyPI page;

[This site](http://vbff.materials.cmu.edu/EMsoft) conveniently brings all EMsoft stuff together in one place.


## Contributors
- [**Zihao Ding**](https://github.com/Darkhunter9)
- [**Marc De Graef**](https://github.com/marcdegraef)


## Installation
The package is built together with the whole **EMsoft** package. Refer to [this site](https://github.com/EMsoft-org/EMsoft) for building directions.

When compiling using `cmake` or `ccmake`, the options `EMsoft_ENABLE_PyEMEBSDDI` and `BUILD_SHARED_LIBS` have to be turned on.

Besides prerequisites in building [**EMsoft**](https://github.com/EMsoft-org/EMsoft) and [**EMsoftSuperbuild**](https://github.com/EMsoft-org/EMsoftSuperbuild), the following packages are also required to be installed:

| Package  | Version  |
| :------------ |:---------------|
| [Python](https://www.python.org/)      | &ge; 3.7    |
| [numpy](https://numpy.org/)            | &ge; 1.18.1 |

Numpy can be installed through pip or conda to current Python environment.

If built successfully, the file `PyEMEBSDDI.so` should be found under `path-to-EMsoftBuild/Bin`.


## How to use?
To use **PyEMEBSDDI**, you may find the following libraries useful:
| Library  | Version  |
| :------------ |:---------------|
| [h5py](http://docs.h5py.org/en/stable/)            | &ge; 2.10.0   |
| [f90nml](https://pypi.org/project/f90nml/)         | &ge; 1.2      |
| [matplotlib](https://matplotlib.org/)              | &ge; 3.2.2    |
| [opencv](https://pypi.org/project/opencv-python/)  | &ge; 4.2.0.34 |

To import **PyEMEBSDDI** into Python program:
```python
import sys
# add path to PyEMEBSDDI.so
sys.path.append("path-to-EMsoftBuild/Bin")

from PyEMEBSDDI import PyEMEBSDDI, PyEMEBSDRefine
```

## API Reference
Currently, the module has 2 methods:

1. PyEMEBSDDI
2. PyEMEBSDRefine 

As the core wrappers, we try to maintain the same arguments as original functions written in Fortran.

`PyEMEBSDDI(ipar, fpar, spar, dpatterns, epatterns, obj=0, cancel=False)`

Dictionary Indexing (DI) function, same as calling `EMEBSDDI` from **EMsoft**.

Input:
- ipar: list of integer parameters, `len(ipar) = 80`.
  Refer to [this](https://github.com/EMsoft-org/EMsoft/blob/c7df98ec74593f83c6d385bf43b68d846ffa08ec/Source/EMsoftWrapperLib/DictionaryIndexing/EMDIwrappermod.f90#L1082) for the meaning of each parameter needed;
- fpar: list of float parameters, `len(fpar) = 80`. No float parameter is needed in this function, so passing `[0.0]*80` is enough;
- spar: list of string parameters, `len(spar) = 80`. The only string parameter used by this function is `spar[22] = OpenCLpathname`;
- dpatterns: dictionary patterns, 2darray, float, (n,numsx*numsy);
- epatterns: experimental patterns, 2darray, float, (n,numsx*numsy);
- obj: int, the function will stop if not `0` (for debugging);
- cancel: bool, the function will stop if not `False` (for debugging);

Output:

`[resultmain, indexmain]`
- resultmain: dot products for top N matches, 2darray, float, (n, TOP_K);
- indexmain: array with indices of matches into the orientations array (corresponding to the orientations of epatterns), 2darray, int, (n, TOP_K);

`PyEMEBSDRefine(ipar, fpar, accum_e, mLPNH, mLPSH, variants, epatterns, startEulers, startdps, obj=0, cancel=False)`

Fit orientation function, same as calling `EMFitOrientation` from **EMsoft**.

Input:
- ipar: list of integer parameters, `len(ipar) = 80`.
  Refer to [this](https://github.com/EMsoft-org/EMsoft/blob/c7df98ec74593f83c6d385bf43b68d846ffa08ec/Source/EMsoftWrapperLib/DictionaryIndexing/EMDIwrappermod.f90#L1596) for the meaning of each parameter needed;
- fpar: list of float parameters, `len(fpar) = 80`.
  Refer to [this](https://github.com/EMsoft-org/EMsoft/blob/c7df98ec74593f83c6d385bf43b68d846ffa08ec/Source/EMsoftWrapperLib/DictionaryIndexing/EMDIwrappermod.f90#L1618) for the meaning of each parameter needed;
- accum_e: array with Monte Carlo histogram, 3darray, float;
- mLPNH: northern hemisphere master pattern, 3darray, float;
- mLPSH: southern hemisphere master pattern, 3darray, float;
- variants: variants array with quaternions defining the potential pseudosymmetry variants float, 2darray, float, (4,nvars) (`np.array([[1,0,0,0]])` if not involved with pseudosymmetry);
- epatterns: experimental patterns, 2darray, float, (n,numsx*numsy);
- startEulers: array with initial Euler angle triplets in radians, 2darray, float, (3,totnumexpt);
- startdps: array with initial dot product values, 1darray, float, (totnumexpt,);
- obj: int, the function will stop if not `0` (for debugging);
- cancel: bool, the function will stop if not `False` (for debugging);

Output:

`[eumain, dpmain]`
- eumain: array with refined Euler angle triplets in radians, 2darray, float(3,totnumexpt)
- dpmain: array with refined dot product values, 1darray, float, (totnumexpt);


<!-- ## Higher-level Python wrappers -->


## Contribute
Have you spotted a typo, would you like to fix a bug, or is there something you’d like to suggest? You are welcome to open a pull request. We will do our best to review your proposal in due time.

In addition, you can also email [**Zihao**](mailto:ding@cmu.edu) should you have any questions or suggestions.

## Credits
We want to express our sincere thanks to those who provided help during the development of this project (in no particular order):

- Saransh Singh
- Elena Pascal
- Michael Jackson

## License
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
