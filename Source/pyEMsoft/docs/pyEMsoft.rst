pyEMsoft
===========================================

Executing EMsoft program in Python
--------------------------------------------
.. role:: bash(code)
   :language: bash

This is directly calling the built EMsoft within Python. First, the .../EMsoftBuild/Release/Bin folder needs to be added to the path. In the terminal, run the python interpreter by typing :bash:`Python`. If you have 
multiple versions of python installled, specify the version as well (e.g. python3.7). In the example below, Python is runing under the Anaconda environment (base).


.. code-block:: bash

    (base) ChaoyideAir:source chaoyizhu$ python3.7
    Python 3.7.3 (default, Mar 27 2019, 16:54:48) 
    [Clang 4.0.1 (tags/RELEASE_401/final)] :: Anaconda, Inc. on darwin
    Type "help", "copyright", "credits" or "license" for more information.
    >>> import os
    >>> os.system('EMmkxtal')

    Copyright (C) 2001-2019 Marc De Graef Research Group/CMU
    EMsoft comes with ABSOLUTELY NO WARRANTY.
    This is free software, and you are welcome to redistribute it
    under certain conditions; see License.txt file for details.


    Program name         : EMmkxtal.f90
    Purpose              : Create an HDF crystal structure file and place it in the XtalFolder
    Platform             : Darwin
    Source code version  : 4_3_0_0
    Source code Revision : 473fc51d58f4ce21e79368757e6032ee5bc10fd4
    Build Date/Time      : 2019-08-10 20:18:23Z

    See https://github.com/EMsoft-org/EMsoft/wiki for selected help pages.

    Aug 29 2019   1:39:02.777 PM


    Select the crystal system :
    1. Cubic
    2. Tetragonal
    3. Orthorhombic
    4. Hexagonal
    5. Trigonal
    6. Monoclinic
    7. Triclinic

    Note about the trigonal system:
    -------------------------------
    Primitive trigonal crystals are defined with respect to a HEXAGONAL
    reference frame.  Rhombohedral crystals can be referenced with
    respect to a HEXAGONAL basis (first setting), or with respect to
    a RHOMBOHEDRAL basis (second setting).  The default setting for
    trigonal symmetry is the hexagonal setting.  When you select
    crystal system 5 above, you will be prompted for the setting.


    crystal system ---> 


How to use the pyEMsoft module? 
-------------------------------------------------
.. role:: python(code)
   :language: python 

In a terminal, cd into the folder containing the shared library (mac: .so file and pyEMsoft.py). Run Python interpreter and then :python:`import pyEMsoft`.

For Python scripts saved in a different folder to the generated shared library and pyEMsofy.py. The directory must be set first to the point to the correct folder.

In an example below, the shared libray and the pyEMsoft.py are saved in a different folder called source. 


.. code-block:: python

   import sys
   sys.path.append('../source')

Typedefs (typedefs.f90)
-------------------------------------------------

It contains definition of all variables and types for crystallographic computations in EMsoft. More specifically, it defines the unitcell type and the orientation type, as well as 
the main cell variable used by all crystallographic computations.

For instance, one important cell variable that is used extensively within EMsoft is the unitcell type.  

.. code-block:: python

    import pyEMsoft 
    LatCell=pyEMsoft.Typedefs.unitcell()

This can now be used as input in other assocaited routines.


Quaternions (quaternions.f90)
-------------------------------------------------

Quaternions module contains basic quaternion functions (some overloaded operators). Quaternions are defined as arrays of 4 single or double precision reals; the first entry is the scalar part, the remaining three form the vector part.

One functions defined in the Quaternions module is to determine the norm of a given quaternion.

.. code-block:: python

   import pyEMsoft 
   import numpy as np

   # define an arbitrary quaternion (double precision)
   q = np.asarray([1, 2, 3, 4], dtype=np.float64) 

   # use the Quaternions module to find norm of the quaternion and obtain a normalized quaternion
   q  = q / pyEMsoft.Quaternions.cabs(q)


Rotations (rotations.f90)
---------------------------------------------

This Rotations module contains everything that has to do with rotations and conversions between rotations. Details regarding this can be referred to the book [1]_ or a more
recent tutorial paper [2]_

For conversion from quaternion to orientation matrix, the qu2eu function can be called:

.. code-block:: python

   import pyEMsoft 
   import numpy as np

   # define an arbitrary quaternion (double precision)
   q = np.asarray([1, 2, 3, 4], dtype=np.float64) 

   # use the Quaternion module to find norm of the quaternion and obtain a normalized quaternion
   q  = q / pyEMsoft.Quaternions.cabs(q)
   
   # determining the corresponding orientation matrix of the arbitrary quaternion
   om = pyEMsoft.Rotations.qu2om(q)


Math (math.f90)
------------------------------------

Math module is a collection of mathematical/numerical routines.

Polar decomposition of a `deformation gradient tensor <https://www.continuummechanics.org/deformationgradient.html>`_ can be carried out using the getpolardecomposition function in the math module.

Polar decomposition is one of the special cases (also the minvert function) where a matrix needs to be allocated in python but the results are filled in the fortran code.

.. code-block:: python

    >>> import pyEMsoft
    >>> import numpy as np
    >>> # define the deformation gradient tensor 
    >>> F=np.array([[1, 0.495, 0.5],[-0.333,1,-0.247],[0.959,0,1.5]], dtype=np.double)
    >>> Rmatrix= np.asfortranarray(np.zeros([3, 3]), dtype=np.double)
    >>> Smatrix= np.asfortranarray(np.zeros([3, 3]), dtype=np.double)
    >>> pyEMsoft.Math.getpolardecomposition(F, Rmatrix, Smatrix)
    >>> print('Polar decomposition of:\n',F, '\n\ngives rotation matrix\n',Rmatrix,'\n\nand stretch matrix\n',Smatrix)
    Polar decomposition of:
    [[ 1.     0.495  0.5  ]
    [-0.333  1.    -0.247]
    [ 0.959  0.     1.5  ]] 

    gives rotation matrix
    [[ 0.91432887  0.37693049 -0.14807474]
    [-0.37389189  0.92618061  0.04893185]
    [ 0.15558786  0.01062414  0.98776492]] 

    and stretch matrix
    [[ 1.18804362  0.0787009   0.78289752]
    [ 0.0787009   1.11276121 -0.02436515]
    [ 0.78289752 -0.02436515  1.39552385]]

In addition, one can check the `flags <https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.flags.html>`_ (part of numpy) for the Rmatrix or the Smatrix. In this case, Rmatrix is in a single, Fortran-style contiguous segment.

.. code-block:: python

    >>> print('\n Rmatrix FLAGS:\n', Rmatrix.flags, '\n')
    Rmatrix FLAGS:
    C_CONTIGUOUS : False
    F_CONTIGUOUS : True
    OWNDATA : True
    WRITEABLE : True
    ALIGNED : True
    WRITEBACKIFCOPY : False
    UPDATEIFCOPY : False 

Crystal (crystal.f90)
-----------------------------------------

The Crystal module includes distance and angle computations, coordinate transformations,normalizations, dot and cross products, generation of asymmetric positions; 
also some routines that deal with reading lattice parameters and atom coordinates and such. 

Given the space group of a crystal, we can find out the corresponding crystal system:

.. code-block:: python

    >>> import pyEMsoft
    >>> pyEMsoft.Crystal.getemsoftxtalsystem(225)
    1
     

To display the periodic table, a displayelements function can be called from the crystal module.

.. code-block:: python

    >>> import pyEMsoft
    >>> pyEMsoft.Crystal.displayelements()

    ------------------------------------ Periodic Table of the Elements --------------------------------------

    1:H                                                                                                    2:He
    3:Li  4:Be                                                               5:B   6:C   7:N   8:O   9:F  10:Ne
    11:Na 12:Mg                                                             13:Al 14:Si 15:P  16:S  17:Cl 18:Ar
    19:K  20:Ca 21:Sc 22:Ti 23:V  24:Cr 25:Mn 26:Fe 27:Co 28:Ni 29:Cu 30:Zn 31:Ga 32:Ge 33:As 34:Se 35:Br 36:Kr
    37:Rb 38:Sr 39:Y  40:Zr 41:Nb 42:Mo 43:Tc 44:Ru 45:Rh 46:Pd 47:Ag 48:Cd 49:In 50:Sn 51:Sb 52:Te 53: I 54:Xe
    55:Cs 56:Ba ----- 72:Hf 73:Ta 74:W  75:Re 76:Os 77:Ir 78:Pt 79:Au 80:Hg 81:Tl 82:Pb 83:Bi 84:Po 85:At 86:Rn
    87:Fr 88:Ra -----
    57:La 58:Ce 59:Pr 60:Nd 61:Pm 62:Sm 63:Eu 64:Gd 65:Tb 66:Dy 67:Ho 68:Er 69:Tm 70:Yb 71:Lu
    89:Ac 90:Th 91:Pa 92:U
    ----------------------------------------------------------------------------------------------------------
    
A more complicated scenario involves use of unitcell defined in the Typedefs class. The following example uses the unitcell as an input/output [(]intent(inout)] in the 
getlatparm function to define crystal structure and lattice parameters/angles.

.. code-block:: python

    import pyEMsoft 
    LatCell=pyEMsoft.Typedefs.unitcell()
    pyEMsoft.Crystal.getlatparm(LatCell)

Symmetry (symmetry.f90)
-----------------------------------------

The Symmetry module deals with all symmetry-related routines. This includes routines to generate a space group based on the generator string; computation
of orbits and families; computation of all atoms in a single or multiple unit cells.

In combination with some functions in the Crystal module. A unitcell containing all the crystllographic information can be generated.

.. code-block:: python

    import pyEMsoft 

    # define the unitcell usinge typedefs
    LatCell=pyEMsoft.Typedefs.unitcell()
    # set the crystal system and lattice parameters/angles
    pyEMsoft.Crystal.getlatparm(LatCell)
    # set the space group number
    pyEMsoft.Symmetry.getspacegroup(LatCell)
    # set space group setting
    pyEMsoft.Symmetry.generatesymmetry(LatCell,True)
    # set atom types, fractional coordiantesm, site occupation and Debye-Waller factor
    pyEMsoft.Crystal.getasympos(LatCell)

    # print the unitcell containing crystallographic information
    print('\nCrystal System:',LatCell.xtal_system,'\n')
    print('Lattice Parameters:\na= %.04f nm\nb= %.04f nm\nc= %.04f nm\n'%(LatCell.a,LatCell.b,LatCell.c),'\n')
    print('Angles:\n\u03B1= %.04f\u00b0\n\u03B2= %.04f\u00b0\n\u03B3= %.04f\u00b0\n'%(LatCell.alpha,LatCell.beta,LatCell.gamma),'\n')
    print('Space Group Number:',LatCell.sym_sgnum, '\n')
    print('Space Group Setting:',LatCell.sym_sgset,'\n')
    print('Number of Atom Types',LatCell.atom_ntype,'\n')
    print('Atom Type:',LatCell.atom_type[0:LatCell.atom_ntype],'\n')
    print('Atom Position',LatCell.atom_pos[0:LatCell.atom_ntype,0:5],'\n')

.. [1] Morawiec, A., 2003. Orientations and rotations. Springer-Verlag.
.. [2] Rowenhorst, D., Rollett, A.D., Rohrer, G.S., Groeber, M., Jackson, M., Konijnenberg, P.J. and De Graef, M., 2015. Consistent representations of and conversions between 3D rotations. Modelling and Simulation in Materials Science and Engineering, 23(8), p.083501.
