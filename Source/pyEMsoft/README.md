# pyEMsoft

> pyEMsoft is a Python interface to the many Fortran types and subroutines in 
[EMsoft](https://github.com/EMsoft-org/EMsoft).

> An extension module is created automatically with [f90wrap](https://github.com/marcdegraef/f90wrap) that can be directly imported in Python `from EMsoft import pyEMsoft`.

## Installation

> Details on installation can be found in [readthedocs](https://pyemsoftreadthedocs.readthedocs.io/en/latest/Installation.html).

- Install [EMsoft_SDK](https://github.com/EMsoft-org/EMsoftSuperbuild)
- Clone [EMsoftData](https://github.com/EMsoft-org/EMsoftData)
- Compile [EMsoft](https://github.com/EMsoft-org/EMsoft) onto your local computer
- Install python 3.x (other python packages are needed to run examples include: jupyter notebook, pyyaml, scikit-image, h5py, matplotlib, ipywidgets)
- Install [f90wrap](https://github.com/marcdegraef/f90wrap) (WSL/Ubuntu/macOS)
- Use `run_pyEMsoft.sh` (located in the EMsoft Build folder) to generate pyEMsoft extension module (*.so and pyEMsoft.py)
- Install the pyEMsoft in the pyEMsoftBuild folder with `python setup.py install`

## How to use?

To import **pyEMsoft** into Python program:

```python
# import the pyEMsoft module
from EMsoft import pyEMsoft
# import utility module
from EMsoft import pyEMsoftTools
```

## Functions

pyEMsoft has access to some of the functions in EMsoft including:

- physical constants (ionic radii, Planck constant, Boltzmann constant, atomic weights, element symbols, etc)
- crystallographic symbols and point/space groups
- quaternion computations (normalization, multiplication, devision, complex conjugate, inner product, inter-quaternion angle, quaterion rotation, interpolation between two quaternions, random quaternion generator, etc)
- conversion between different rotation representations (axis-angle pair, Rodrigues-Frank vector, quarterions, stereographic vector, homochoric vector, cubochoric vector, Euler angles, orientation matrix)
- HDFsupport to export or import EMsoft specific HDF5 data sets (crystal.xtal, EBSDmaster.h5, EBSD_MonteCarloData.h5)
- calculate allowed diffraction vectors
- access to diffraction group, crystal point group, Laue group, projection diffraction group and many 2D symmetry point groups (for bright field, dark field, whole pattern diffraction) based on crystal structure data (.xtal), crystal point group number, zone axis ([uvw]) information.
- Lambert square projection (and its inverse)
- diffraction angle (2 theta) of a plane
- electron's physical properties based on SEM's accelerating voltage
- scattering factor sets, mean inner potential, interaction constant for a given crystal structure file (.xtal)
- etc.

## Contributors

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
- Zihao Ding
- Marcus Ochsendorf
- Christian Kurniawan

## Financial Support

EBSD/ECP/EKP development of this package, including dictionary indexing for EBSD/ECP, was started with support from 
an AFOSR/MURI grant, FA9550-12-1-0458; the original EBSD code from CTEMsoft 2.0 was developed with support from 
an ONR grant, N00014-12-1-0075. All recent development of TKD and related modalities, including the creation of 
routines that can generate PoVRay visualization script files, was performed with support from an ONR Vannevar Bush 
Fellowship, N00014-­16-­1-­2821.