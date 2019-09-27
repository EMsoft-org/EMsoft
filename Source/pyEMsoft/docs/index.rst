pyEMsoft's Documentation
====================================

What is pyEMsoft?
------------------------------------
`EMsoft <https://github.com/EMsoft-org/EMsoft>`_ is an open source program for computation and visualization 
of scanning electron microscopy diffraction patterns such as EBSD,ECP,TKD, and EKP. pyEMsoft is a Python interface 
to EMsoft that provides some access of the Fortran types and subrouines. pyEMsoft is automatically generated from a 
Python interface generator for Fortran (`f90wrap <https://github.com/jameskermode/f90wrap>`_).

Financial Support
------------------------------------
EBSD/ECP/EKP development of this package, including dictionary indexing for EBSD/ECP, was started with support from 
an AFOSR/MURI grant, FA9550-12-1-0458; the original EBSD code from CTEMsoft 2.0 was developed with support from 
an ONR grant, N00014-12-1-0075. All recent development of TKD and related modalities, including the creation of 
routines that can generate PoVRay visualization script files, was performed with support from an ONR Vannevar Bush 
Fellowship, N00014-­16-­1-­2821.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   self
   Installation
   pyEMsoft

.. toctree::
   :maxdepth: 2
   :caption: Modules:

   Modules/pyEMsoftLib
   Modules/pyEMsoftToolsLib

License
------------------------------------
Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University 
All rights reserved.Redistribution and use in source and binary forms, with or without modification, are 
permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
- Neither the names of Marc De Graef, Carnegie Mellon University nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Indices
==================

* :ref:`genindex`
* :ref:`search`