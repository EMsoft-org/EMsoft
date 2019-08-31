Installation
====================================
.. role:: bash(code)
   :language: bash

The pyEMsoft module can only be generated after EMsoft has been built:

- To compile EMsoft, you need to first build the `Software Developer Kit <https://github.com/EMsoft-org/EMsoftSuperbuild>`_ on your operating system. 
- Then, follow the instructions in `EMsoft <https://github.com/EMsoft-org/EMsoft>`_ to compile the Fortran library. 
- Next, complete the EMsoft package configuration following the `EMsoft Wiki Package Configuration <https://github.com/EMsoft-org/EMsoft/wiki/Package-Configuration>`_.
- To test if EMsoft has been configured correctly,run a simple example such as `Crystal Data Entry Example <https://github.com/EMsoft-org/EMsoft/wiki/Crystal-Data-Entry-Example>`_.
- (Suggested) Install a recent version of the `Anaconda <https://www.anaconda.com/distribution/>`_ to get other packages such as Python, numpy, and many other packages.
- :bash:`conda install lapack`. This should automatically install the blas and tmglib as well. These libraries will be linked later to some of the modules for convinient `linear algebra operations <http://www.netlib.org/lapack/>`_.
- Install `f90wrap <https://github.com/jameskermode/f90wrap>`_ and check if f90wrap and f2py-f90wrap have been added to path. These are the two important executables..
- :bash:`cd` into the pyEMsoft folder in the EMsoft repository and execute the shell script with :bash:`./run_pyEMsoft.sh`


Dependences
------------------------------------
* Github
* EMsoft_SKD
* EMsoft
* Python >=2.7 or 3.x (unittest files provided in Python 3.x)
* recent version of numpy which includes support for f2py
* Fortran compiler gfortran 6.3+ or ifort 12+
* f90wrap

Supported Platforms (mac tested)
------------------------------------
+--------------------------------+-----------------------------------------+----------------------------------------+
| Operating System               |        C/C++ Compiler                   |     Fortran Compiler                   |       
+================================+=========================================+========================================+
| macOS (10.12)                  | Xcode Native tools (8.3.x)              | GFortran 6.3.0 and above               | 
+--------------------------------+-----------------------------------------+----------------------------------------+
| Windows (10)                   | Visual Studio 2015 (CE/Pro)             |      Intel Fortran v17                 |
+--------------------------------+-----------------------------------------+----------------------------------------+
| Linux (Ubuntu 16.x, CentOS 7.x)| GCC 4.8 and Above/Clang 3.8 and greater |     GNU Fortran 5.4.1 20160904 or newer|          
+--------------------------------+-----------------------------------------+----------------------------------------+