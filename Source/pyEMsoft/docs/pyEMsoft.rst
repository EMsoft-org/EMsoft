pyEMsoft
===========================================

Executing EMsoft program in Python
--------------------------------------------
.. role:: bash(code)
   :language: bash

This is directly calling the built EMsoft functions from within Python. First, the :bash:`.../EMsoftBuild/Release/Bin` folder needs to be added to the path. Second, complete the EMsoft package configuration following the `EMsoft Wiki Package Configuration <https://github.com/EMsoft-org/EMsoft/wiki/Package-Configuration>`_. 
Then, create a Ni.xtal file as defined in `Crystal Data Entry Example <https://github.com/EMsoft-org/EMsoft/wiki/Crystal-Data-Entry-Example>`_. This crystal file
will later be read into some of the unittests files.

.. code-block:: bash

    ChaoyideAir:source chaoyizhu$ python3.7
    Python 3.7.3 (default, Mar 27 2019, 16:54:48) 
    [Clang 4.0.1 (tags/RELEASE_401/final)] 
    Type "help", "copyright", "credits" or "license" for more information.
    >>> import os
    >>> os.system('EMmkxtal')


How to use the pyEMsoft module? 
-------------------------------------------------
.. role:: python(code)
   :language: python 

In a terminal, cd into the folder containing the shared library (mac: .so file and pyEMsoft.py). Run Python interpreter and then :python:`import pyEMsoft`. For Python scripts saved in a different folder to the generated shared library and pyEMsoft.py (e.g. unittests folder). The directory must be set first to the point to the correct folder.In an example below, the shared library and the pyEMsoft.py are saved in a different folder (one folder level up).In addition, some simple tools dedicated to :python:`pyEMsoft` have also been created and are saved inside a separate module named :python:`pyEMsoftTools`. Depending on the need, some other popular modules will also be imported such as the :python:`numpy`, :python:`math`, :python:`unittest`, :python:`random`, etc. 


.. code-block:: python

   import sys
   sys.path.append('../')
   import pyEMsoft
   from pyEMsoftTools import Tools
   import numpy as np



Constants (constants.f90)
-------------------------------------------------

This module contains physical and mathematical constants used by various programs 
such as periodic table information, atomic weights, etc.

A few examples have been tested in the unittest file. For instance, 
several examples of basic constants (in SI units) can be found in this module:

.. code-block:: python

        # value of pi, speed of light, Planck constant, Boltzmann constant
        print('Value of \u03C0 is %.19f' % (pyEMsoft.constants.cpi), '\n')
        print('Speed of light (c) is %.1f (m/s)' % (pyEMsoft.constants.clight), '\n')
        print('Planck constant (h) is %e (Js)' % (pyEMsoft.constants.cplanck), '\n')
        print('Boltzmann Constant (k) is %e (mˆ2kgsˆ(-1)Kˆ(-1))' % (pyEMsoft.constants.cboltzmann), '\n')

For a complete list of constants, please refer to the original fortran file (constants.f90).

Typedefs (typedefs.f90)
-------------------------------------------------

It contains definition of all variables and types for crystallographic computations in EMsoft. More specifically, it defines the unitcell type and the orientation type (class type objects in python) , as well as the main cell variable used by all crystallographic computations.

For instance, one important variable that is used extensively within EMsoft is the :python:`unitcell` type.  

.. code-block:: python

    # use the unitcell class defined in the typedefs module
    LatCell=pyEMsoft.typedefs.unitcell()

This :python:`unitcell` can now be used as input/output variable in other associated routines.

Another special case of variable is the character array such as a list of space group name. The output numpy.array is in ASCII encoded format. To retrieve the characters, get_character_array function from the pyEMsoftTools can be used.

.. code-block:: python

    # convert the space group names from ASCII encoded numpy array to character array
    Tools.get_character_array(pyEMsoft.typedefs.sym_sgname)

Quaternions (quaternions.f90)
-------------------------------------------------

Quaternions module contains basic quaternion functions such as quaternion normalization/complex conjugate/multiplication/division/inner product/rotation, interquaternion angle, random quaternion and interpolation between quaternions.

One function defined in the Quaternions module is to determine the norm of a given quaternion.

.. code-block:: python

    # define an arbitrary quaternion (single precision)
    q = np.asarray([1, 2, 3, 4], dtype=np.float32) 
    # use the Quaternions module to find norm of the quaternion and obtain a normalized quaternion
    q  = q / pyEMsoft.quaternions.cabs(q)
    # check the precision
    print(q.dtype)

Note that the interface function e.g. :python:`cabs` have both single precision (:python:`_quat_norm`) and double precision (:python:`_quat_norm_d`) routines 'glued together' in the Fortran script. Passing either single precision array (float32) or double precision array (float64) will default into the first single precision routine :python:`_quat_norm` (at the moment)unless the :python:`_quat_norm_d` is explicitly defined.

.. code-block:: python

    # define an arbitrary quaternion (double precision)
    q = np.asarray([1, 2, 3, 4], dtype=np.float64) 
    # use the Quaternions module to find norm of the quaternion and obtain a normalized quaternion
    q  = q / pyEMsoft.quaternions._quat_norm_d(q)
    # check the precision
    print(q.dtype)

Rotations (rotations.f90)
---------------------------------------------

This Rotations module contains everything that has to do with rotations and conversions between rotations. Details regarding this can be referred to the book [1]_ or a more recent tutorial paper [2]_

For conversion from quaternion to orientation matrix, the :python:`qu2eu` function can be called:

.. code-block:: python

    # define an arbitrary quaternion (single or double precision)
    q = np.asarray([1, 2, 3, 4], dtype=np.float32) 
    # use the Quaternion module to find norm of the quaternion and obtain a normalized quaternion
    q  = q / pyEMsoft.Quaternions.cabs(q)
    # determining the corresponding orientation matrix of the arbitrary quaternion
    om = pyEMsoft.Rotations.qu2om(q)

To see if the lapack library is correctly linked, you can check if the :python:`om2ax` routine outputs the correct value because it uses lapack to calculate the eigenvalue of a given matrix. A specific unittest is added in the test_rotations.py file to for the verification of the lapack library.

In the rotations module, the :python:`init_orientation` and :python:`init_orientaiton_om` functions can be used to communicate with all the rotation conversion functions in the rotations module. By providing a random quaterion and looping over the rotation methods, all the rotation conversion functions can be tested. A double precision quaternion is defined first for the following example (see unittest script test_rotations.py):

.. code-block:: python

    # define some rotation methods (om has its dedicated routine)
    # qu is not included because we are using it as a inputtype
    # and qu2om, qu2eu, ... can be used to 
    rotation_method = ['qu','eu', 'ax', 'ro', 'ho', 'cu', 'st','om']
    # get function from the pyEMsoft.rotations module
    def get_function(str):
        return getattr(pyEMsoft.rotations, str)
    # loop over the rotation method list
    for i in rotation_method:
        # qu is already an input so it does not require conversion
        if i == 'qu':
            res = pyEMsoft.rotations._init_orientation_d(self.q, i, rotcheck=False)
            print(i, 'to other types\n', res,'\n')
        else:
            f = get_function('_qu2'+i+'_d')
            inputtype = f(self.q)
            # init_orientation_om is a separate function
            if i == 'om':
                res = pyEMsoft.rotations._init_orientation_om_d(inputtype, i, rotcheck=True)
                print(i, 'to other types\n', res, '\n')
            else:
                res = pyEMsoft.rotations._init_orientation_d(inputtype, i, rotcheck=False)
                print(i, 'to other types\n', res, '\n')

In addition, functions in the rotations module involving checking the bound and norm of a given type of rotation, coordinate transformation of tensor and vectors (passive or active transformation), and a way of computing the geometrical mean
of a list of quaternions (including its standard deviation quaternion).

Math (math.f90)
------------------------------------

Math module is a collection of mathematical/numerical routines. For instance, mathematical operations to obtain matrix inverse, cross-product, matrix determinant, cubic roots, etc.

Polar decomposition of a `deformation gradient tensor <https://www.continuummechanics.org/deformationgradient.html>`_ can be carried out using the getpolardecomposition function in the math module.

Polar decomposition is one of the special cases (also the minvert function) where a matrix needs to be preallocated in python but the results are filled in the fortran code. The preallocation essentially defines an empty numpy array in Fortran order in memory. 

.. code-block:: python

    >>> # define the deformation gradient tensor 
    >>> F=np.array([[1, 0.495, 0.5],[-0.333,1,-0.247],[0.959,0,1.5]], dtype=np.double)
    >>> Rmatrix= np.asarray(np.zeros([3, 3]), dtype=np.double, order='F')
    >>> Smatrix= np.asarray(np.zeros([3, 3]), dtype=np.double, order='F')
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

In addition, one can check the `flags <https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.flags.html>`_ (part of numpy) for 
the Rmatrix or the Smatrix. In this case, Rmatrix is in a single, Fortran-style contiguous segment.

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

Whenever the output variable is preallocated in python, the interface function such as :python:`minvert` can now switch between the single precision (:python:`_minvert`) and double precision routines (:python:`_minvert_d`), depending on the precision of the preallocated out variable. The try and except python function is wrapped around these routines of different precisions such that the prompted :python:`ValueError` is used as a switch. 

HDFsupport (HDFsupport.f90)
-----------------------------------------

This module contains some of the HDF5 helper routines that can be used to export or import HDF5 data set. The routines within HDFsupport can already read/write EMsoft specific format data. This is probably more convenient than writing hdf5 specific module (see Example4_read_crystal_data)

One example routine from this module is able to save crystal data unitcell (Example 1 below shows how to create a crystal unitcell in python) to a .xtal file in the default XtalFolder (EMsoft package configuration is required). 

.. code-block:: python

    # use routine from HDFsupport to save crystal data
    pyEMsoft.hdfsupport.savedatahdf(LatCell)

In addition, it is also possible to read crystal data from a .xtal file from the XtalFolder using :python:`crystaldata` funciton, written based on the :python:`readdatahdf` function from the same HDFsupport module. Within this :python:`crystaldata` function, addtional function :python:`calcmatrices` (from crystal.f90) is called to 
computes the direct/reciprocal lattice/structure matrices for a given crystal.

.. code-block:: python

    # readin the existing hdf5 data (in the XtalFolder)
    pyEMsoft.hdfsupport.crystaldata(LatCell)

EBSDmod (EBSDmod.f90)
-----------------------------------------
This module contains several functions to work with EBSD input and output data. For instance, we can use it to read in a list of Euler angles, Monte Carlo data and master pattern data. 

A list of Euler angles (.txt) needs to be first created in the EMsoft data folder (EMdatapathname). In the unittests file, the euler.txt is created, which contains two sets of Euler angles. 

.. code-block:: python

	# EBSD name list types
    enl = pyEMsoft.namelisttypedefs.EBSDNameListType()
    # define name of the file containing angles
    enl.anglefile='euler.txt'
    enl.eulerconvention='hkl'
    #enl.anglefiletype = 'orientations'
    angles=pyEMsoft.ebsdmod.EBSDAngleType()
    # verbose=True converts eu to qu, hkl to tsl
    numangles = pyEMsoft.ebsdmod.ebsdreadangles(enl,angles,verbose=True)
    # the quaternions are saved in columns
    print(angles,'\n')


It is required to first open the hdf5 interface through the :python:`h5open_emsoft` (HDFsupport module) before we can use these functions such as :python:`readebsdmasterpatternfile`.

.. code-block:: python

    # MPfile=input('Master pattern file (path relative to EMdatapathname):')
    MPfile = 'Ni-master-20kV.h5'
    # master pattern namelist types
    mpnl = pyEMsoft.namelisttypedefs.EBSDMasterNameListType()
    # Monte Carlo namelist types
    mcnl = pyEMsoft.namelisttypedefs.MCCLNameListType()
    # master pattern data types
    EBSDMPdata = pyEMsoft.typedefs.EBSDMPdataType()
    # Monte Carlo data types
    EBSDMCdata = pyEMsoft.typedefs.EBSDMCdataType()
    # hdferror (inout int), hdferror=0, no error; hdferror=1 means error returned
    hdferr = np.asarray([0], dtype=int, order='F')
    # open the hdf5 interface first
    pyEMsoft.hdfsupport.h5open_emsoft(hdferr)
    # readebsdmasterpatternfile is a Fortran routine that exports all relevant information from
    # Monte Carlo data. The following example shows how to get accum_e
    pyEMsoft.ebsdmod.readebsdmontecarlofile(MPfile, mcnl, EBSDMCdata, getaccume=True)
    # readebsdmasterpatternfile is a Fortran routine that exports all the information from the master pattern
    # if keep4=True, this keeps the original rank 4 tensor can has an extra dimension associated with atom types
    # (e.g.EBSDMPdata.mlpnh4, EBSDMPdata.mlpsh4 )
    pyEMsoft.ebsdmod.readebsdmasterpatternfile(MPfile, mpnl, EBSDMPdata, 
    getmlpnh=True, getmlpsh=True, getmasterspnh=True, getmasterspsh=True)
    pyEMsoft.hdfsupport.h5close_emsoft(hdferr)


Crystal (crystal.f90)
-----------------------------------------

The Crystal module includes distance and angle computations, coordinate transformations, normalizations, dot and cross products, generation of asymmetric positions; also some routines that deal with reading lattice parameters and atom coordinates and such. 

Given the space group of a crystal, we can find out the corresponding crystal system using the :python:`getemsoftxtalsystem`:

.. code-block:: python

    # input a space group for fcc crystal (should be in the cubic crystal system=1)
    >>> pyEMsoft.crystal.getemsoftxtalsystem(225)
    1
     

To display the periodic table, a :python:`displayelements` function can be called from the crystal module. This routine simply uses message routine defined in io.f90
to directly print out strings to the terminal. 

.. code-block:: python

    # display the elements in the periodic table 
    pyEMsoft.crystal.displayelements()

A more complicated scenario involves use of :python:`unitcell` defined in the Typedefs module. The following example uses the unitcell as an input/output [intent(inout)] in the 
:python:`getlatparm` function to define crystal structure and lattice parameters/angles.

.. code-block:: python

    LatCell=pyEMsoft.typedefs.unitcell()
    pyEMsoft.crystal.getlatparm(LatCell)

 The crystal structure information can be obtained in two ways: 1) either read from an existing .xtal file (as in the unittest file), 2) or go through the steps in Example1_make_crystal.py.

.. code-block:: python

    # a crystal unitcell needs to be created before testing the routines
    # define the unitcell using typedefs to store crystallographic data
    LatCell = pyEMsoft.typedefs.unitcell()
    # file name of the crystal data file
    LatCell.fname = 'Ni.xtal'
    # readin the existing hdf5 data (in the XtalFolder)
    # this function also uses readDataHDF (HDFsupport.f90) and CalcMatrices (crystal.f90)
    pyEMsoft.hdfsupport.crystaldata(LatCell)

In some cases, the direct lattice vectors may need to be transformed to reciprocal space or cartesian reference frame. The :python:`transspace` routine can be used to convert a vector between the three spaces with a single character as a switch: direct space ('d'), reciprocal space ('r'), cartesian reference frame ('c').

.. code-block:: python

    # define an arbitrary input vector in the direct space
    input_vector = np.asarray([1, 1, 1], dtype=np.float32)
    # define the output array first in fortran order in memory
    output_vector = np.asarray([0, 0, 0], dtype=np.float32, order='F')
    # define the space of input vector (direct space)
    inspace = 'd'
    # define hte space of the output vector (standard cartesian reference frame)
    outspace = 'r'
    # now call the transspace from crystal module to convert the input vector into another space
    pyEMsoft.crystal.transspace(self.LatCell, input_vector, output_vector, inspace, outspace)
    print('The', input_vector, 'in the ', Tools.get_space_string(inspace), 'has been converted to', output_vector,'in', Tools.get_space_string(outspace), '\n')

If coordinate transformation is needed in a given space ('d', 'c' or 'r'), the :python:`trnascoor` function can be called to defined a coordinate transformed
vector from old to new ('on') or new to old ('no').

.. code-block:: python

    # vector components involving a transformation matrix 
    # define the output array first in fortran order in memory (double precision)
    output_vector_t = np.asarray([0, 0, 0], dtype=np.float64, order='F')
    # the transformation here is defined from a random quaternion (ideally this should 
    # be matrix with directional cosines formed by the basis vectors of two coordinates systems (old and new)
    trans_m = pyEMsoft.rotations.qu2om(self.q)
    # call the transcoor function for the coordinate transformation in a defined space ('on'=old to new, 'no'=new to old)
    pyEMsoft.crystal.transcoor(self.LatCell, output_vector, output_vector_t, trans_m, 'c', 'on')
    print('The output vector is', output_vector_t, 'under the transformation matrix\n', trans_m, '\n')  

Furthermore, there is a module called :python:`milbrav` to help with conversion between Miller indices and Miller-Bravais indices ('34' or '43' is the switch). 

.. code-block:: python

    # first we do a Miller to Miller-Bravais indices conversion (switch:'34')
    Miller_index = np.asarray([1, 0, 1], dtype=np.int32)
    Miller_Bravais_index = np.asarray([0, 0, 0, 0], dtype=np.int32, order='F')
    pyEMsoft.crystal.milbrav(Miller_index, Miller_Bravais_index, '34')
    print('Miller indices', Miller_index, 'is converted to Miller-Bravais indices:', Miller_Bravais_index,'\n')
    # then we do a Miller-Bravais to Miller indices conversion (switch:'43')
    Miller_index = np.asarray([0, 0, 0], dtype=np.int32, order='F')
    pyEMsoft.crystal.milbrav(Miller_index, Miller_Bravais_index, '43')
    print('Miller-Bravais indices', Miller_Bravais_index, 'is converted to Miller indices:', Miller_index,'\n')

To obtain density of a crystal structure (and average atomic weight), we can first find all the atom positions in a unit cell (symmetry.calcpositions) and then use 
:python:`calcdensity` from the crystal module.

.. code-block:: python

    # calculate positions of atoms in the unit cell
    pyEMsoft.symmetry.calcpositions(self.LatCell, 'v')
    # calculate density, average atomic number, average atomic weight
    density, avZ, avA = pyEMsoft.crystal.calcdensity(self.LatCell)
    print(density, avZ, avA)
    print('Density=', density, '(g/cm^3)', 'average atomic number=', avZ, 'average atomic weight=', avA, '(g/mol)\n')
    print('unit cell volume', self.LatCell.vol)

Moreover, the Crystal module contains many other useful tools to work with crystallography such vector normalization, length of vector, angle between
vectors, cross product of two vectors, etc for any given space. An example is given below in Example2_basic_crystallography to solve some of the problems in the textbook written by Marc De Graef [3]_

Symmetry (symmetry.f90)
-----------------------------------------

The Symmetry module deals with all symmetry-related routines. This includes routines to generate a space group based on the generator string; computation of orbits and families; computation of all atoms in a single or multiple unit cells. 

In the corresponding unittest file (test_symmetry.py), the crystal structure information is directly read from an existing Ni.xtal file (if this does not exist, you need to create one).

The :python:`isgallowed` function helps to determine whether an input (integer array) reciprocal lattice vector is forbidden due to precense of a certain type of atom centering.

.. code-block:: python

    # define three vectors in reciprocal space (integer arrays)
    g1 = np.array([1, 1, 1])
    g2 = np.array([1, 0, 1])
    g3 = np.array([2, 2, 4])
    # decode the bytes to utf-8 strings to get space symbols
    space_group_name = (self.LatCell.sg.sym_name).decode('utf-8')
    print('Is reflection g1', g1, 'allowed in', '\'', space_group_name[1], 
    '\'', 'type centering?')
    print('Answer:', bool(pyEMsoft.symmetry.isgallowed(self.LatCell, g1)), 
    '\n')
    print('Is reflection g2', g2, 'allowed in', '\'', space_group_name[1], 
    '\'', 'type centering?')
    print('Answer:', bool(pyEMsoft.symmetry.isgallowed(self.LatCell, g2)), 
    '\n')
    print('Is reflection g3', g3, 'allowed in', '\'', space_group_name[1], 
    '\'', 'type centering?')
    print('Answer:', bool(pyEMsoft.symmetry.isgallowed(self.LatCell, g3)), 
    '\n')

With the :python:`getpatternsymmetry` function, diffraction group, crystal point group, Laune group, projection diffraction group, and many 2D symmetry point groups (for bright field, dark field, whole pattern diffraction) can be accessed through some input variable e.g. crystal structure data (unitcell), crystal point group number, and zone axis ([uvw]).

Another useful routine is the :python:`calcpositions`, which is used to calculate a list of atom positions for every atom type in the crystal. For example, this is used in :python:`calcdensity`.

Lambert (lambert.f90)
-----------------------------------------

This module contains a number of projection functions for the modified
Lambert projection between square lattice and 3D hemisphere [4]_, hexagonal lattice
and 3D hemisphere, as well as the more complex mapping between a 3D cubic grid
and the unit quaternion hemisphere with positive scalar component.  In addition, there
are some other projections, such as the stereographic ones.  Each function is named
by the projection, the dimensionality of the starting grid, and the forward or inverse
character.  For each function, there is also a single precision and a double precision
version, but we use the interface formalism to have only a single call. The Forward
mapping is taken to be the one from the simple grid to the curved grid. Since the module
deals with various grids, we also add a few functions/subroutines that apply symmetry
operations on those grids. 

.. code-block:: python

    # 2D square coordinates
    xy = np.asarray([0.3, 0.2], dtype=np.float32)
    # return 1 if the point lies outside the bounds
    ierr = np.asarray([0], dtype=np.int32, order='F')
    # 2D square coordinates to 3D hemisphere transformation
    xyz = pyEMsoft.lambert.lambertsquaretosphere(xy, ierr)
    print('2D square coordinates', xy, 'is transformed into 3D coordinates',
    xyz, '\n')
    # 3D hemisphere to 2D square coordinates transformation
    xy = pyEMsoft.lambert.lambertspheretosquare(xyz, ierr)
    print('3D coordinates', xyz, 'is transformed into 2D square coordinates',
    xy, '\n')

An example that involves use of the :python:`lambertsquaretosphere` is given in the jupyter notebook file to plot Kikuchi sphere Example3_plot_Kikuchi_Sphere. First, master lamber projection patterns (weighted by atom occupancy) are weighted average based on Monte Carlo yield. 2D square coordinates are prescaled into the bounds and are then converted into 3D hemispherical coordiantes for the southern and northern 
hemispheress respectively.

Diffraction (diffraction.f90)
-----------------------------------------

This module contains many routines used in the dynamical diffraction. In the unittests script, two classes of derived are used: :python:`unitcell` (crystal) and :python:`gnode` (diffraction related quantaties). Moreover, an example crystal data file (Ni.xtal) is read from the XtalFolder.

Most of the physical quantaties related to dyanmical diffraction can be obtained through the following :python:`getvoltage` function, which communates to many other routines in the diffraction module.

.. code-block:: python

    # get the accelerating voltage, relativistic correction factor
    # relativistic accelerating potential, electron wavelength
    # obtain scattering factor sets, mean inner potential, interaction constant
    # this routine uses many other rountines such as getscatfac, CalcUcg, Calcwavelength
    pyEMsoft.diffraction.getvoltage(self.LatCell, self.rlp, True)
    
From the kinematical diffraction theory, the 2 theta diffraction angle can be calculated for any diffracting plane:


.. code-block:: python

    # calculate 2theta diffraction angle for a plane (hkl)
    Angle = pyEMsoft.diffraction.calcdiffangle(self.LatCell, 1, 1, 1)
    print('\nDiffraction angle for (111) is:', Angle, '(rad)\n')

Examples
-----------------------------------------

The Symmetry module in combination with some functions in the Crystal module. 
A :python:`unitcell` containing all the crystllographic information can be generated. User can either interact with the terminal to populate the unitcell with crystallographic information or define the values in the :python:`unicell` manually e.g. LatCell.xtal_system=1.


Example 1: Make a crystal

.. code-block:: python

    import sys
    sys.path.append('../')
    import pyEMsoft
    # define the unitcell usinge typedefs
    LatCell = pyEMsoft.typedefs.unitcell()
    # set the crystal system and lattice parameters/angles
    pyEMsoft.crystal.getlatparm(LatCell)
    # set the space group number
    pyEMsoft.symmetry.getspacegroup(LatCell)
    # set space group setting
    pyEMsoft.symmetry.generatesymmetry(LatCell, True)
    # set atom types, fractional coordiantesm, site occupation and Debye-Waller factor
    pyEMsoft.crystal.getasympos(LatCell)
    # input file name (.xtal)
    LatCell.fname=input('File name of the crystal file (.xtal):')
    # source of crystal data
    LatCell.source=input('Source of crystal data:')
    # print the unitcell containing crystallographic information
    print('\nCrystal System:', LatCell.xtal_system,'\n')
    print('Lattice Parameters:\na= %.04f nm\nb= %.04f nm\nc= %.04f nm\n' 
    % (LatCell.a, LatCell.b, LatCell.c), '\n')
    print('Angles:\n\u03B1= %.04f\u00b0\n\u03B2= %.04f\u00b0\n\u03B3= %.04f\u00b0\n' 
    % (LatCell.alpha, LatCell.beta, LatCell.gamma),'\n')
    print('Space Group Number:', LatCell.sym_sgnum, '\n')
    print('Space Group Setting:', LatCell.sym_sgset, '\n')
    print('Number of Atom Types', LatCell.atom_ntype, '\n')
    print('Atom Type (atomic number):', LatCell.atom_type[0:LatCell.atom_ntype], '\n')
    print('Fractional coordiantes, Site occupation and Debye-Waller factor: \n', 
    LatCell.atom_pos[0:LatCell.atom_ntype, 0:5], '\n')
    # empty spaces are reserved for more input characters
    print('File name:', LatCell.fname.decode('utf-8'), '\n')
    print('Source of crystal data:', LatCell.source.decode('utf-8'))
    # use routine from HDFsupport to save crystal data in the XtalFolder
    pyEMsoft.hdfsupport.savedatahdf(LatCell)


Example 2: Basic Crystallography Computations

In the following example, several routines in the Crystal module are utilized to solve some simple crystallography problems [3]_ . 

.. code-block:: python

    import sys
    sys.path.append('../')
    import pyEMsoft
    from pyEMsoftTools import Tools
    import numpy as np
    import math

    # Examples here are taken from A textbook written by Professor Marc De Graef:
    # De Graef, M., 2003. Introduction to conventional transmission electron microscopy.
    # Cambridge University Press.

Task 1: a tetragonal crystal has lattice parameters a=1/2 nm and c=1 nm. Compute its metric tensor:

.. code-block:: python

    # Example 1.1
    # define the unitcell usinge typedefs
    LatCell = pyEMsoft.typedefs.unitcell()
    # set the crystal system and lattice parameters/angles 
    # use a=0.5, c=1
    pyEMsoft.crystal.getlatparm(LatCell)
    # calculate the reciprocal/direct metric/structure tensors 
    pyEMsoft.crystal.calcmatrices(LatCell)
    print('\nDirect metric tensor of the tetragonal crystal with a=0.5, c=1 is:\n', 
    np.round(LatCell.dmt,2), '\n')

Task 2: compute the distance between the points (1/2,0,1/2) and (1/2,1/2,0) in direct lattice space for 
the crystal defined in Task 1.

.. code-block:: python

    # Example 1.2
    # distance between the two points is equal to the length of the vector 
    # connecting the two points in direct space
    p1 = np.array([0.5, 0, 0.5])
    p2 = np.array([0.5, 0.5, 0])
    v = p1-p2
    # define the space (d=direct space, r=reciprocal space, c=cartesian reference frame)
    space = 'd'
    v_length = pyEMsoft.crystal.calclength(LatCell, v, space)
    print('Length between', p1, 'and', p2, 'is', v_length, '(nm) in the', 
    Tools.get_space_string(space))

Task 3: compute the dot product and angles between the vectors [120] and [311] for the crystal defined 
in Task 1.

.. code-block:: python

    # Example 1.3 
    # dot product and angle between two vectors in the direct space
    v1 = np.array([1, 2, 0])
    v2 = np.array([3, 1, 1])
    v1_dot_v2 = pyEMsoft.crystal.calcdot(LatCell, v1, v2, space)
    print('Dot product of', v1, 'and', v2, 'is', v1_dot_v2, '(nmˆ2) in the', 
    Tools.get_space_string(space))
    # angle between two vectors in the direct space
    a = pyEMsoft.crystal.calcangle(LatCell, v1, v2, space)
    print('The angle between', v1, 'and', v2, 'is %4f' % math.degrees(a))

Task 4: compute the reciprocal metric tensor for the crystal defined in Task 1
    
.. code-block:: python

    # Example 1.5 & 1.9
    print('\nReciprocal metric tensor of the tetragonal crystal with a=0.5,c=1 is:\n', 
    np.round(LatCell.rmt,2), '\n')
    # The reciprocal lattice vectors are just rows of the the recirpocal metric tensor

Task 5: compute the angle between the (120) and (311) plane normals for the crystal defined in Task 1.

.. code-block:: python

    # Example 1.6
    # given two plane normals 
    n1 = np.asarray([1, 2, 0])
    n2 = np.asarray([3, 1, 1])
    space = 'r'
    a = pyEMsoft.crystal.calcangle(LatCell, n1, n2, space)
    print('The angle between', n1, 'and', n2, 'is %4f' % math.degrees(a), '\n')

Task 6: write down the reciprocal components of the lattice vector [114] for the crystal defined in Task 1.
    
.. code-block:: python

    # Example 1.8
    v = np.asarray([1, 1, 4])
    n = np.asarray([0, 0, 0], dtype=np.float32, order='F')
    inspace = 'd'
    outspace = 'r'
    pyEMsoft.crystal.transspace(LatCell, v, n, inspace, outspace)
    print('The', v, 'in the ', Tools.get_space_string(inspace), 'has been converted to', 
    n,'in', Tools.get_space_string(outspace), '\n')

Task 7: determine the cross product of the vectors [110] and [111] in the crystal defined in Task 1.

.. code-block:: python

   # Example 1.10
    v1 = np.asarray([1, 1, 0], dtype=np.float32)
    v2 = np.asarray([1, 1, 1], dtype=np.float32)
    output_vector = np.asarray([0, 0, 0], dtype=np.float32, order='F')
    # define the space of input vector (direct space)
    inspace = 'd'
    # define the space of the output vector (reciprocal space)
    outspace = 'r'
    pyEMsoft.crystal.calccross(LatCell, v1, v2, output_vector, inspace, outspace, True)
    print('The output vector cross product is', output_vector, 
    'in the', Tools.get_space_string(outspace))
    pyEMsoft.crystal.calccross(LatCell, v1, v2, output_vector, inspace, inspace, True)
    print('or',output_vector,'in the', Tools.get_space_string(inspace), '\n')


Task 8: determine the cross product of the reciprocal lattice vectors [110] and [111] in the crystal defined in Task 1.

.. code-block:: python

    # Example 1.12
    g1 = np.asarray([0, 0, 0], dtype=np.float32, order='F')
    g2 = np.asarray([0, 0, 0], dtype=np.float32, order='F')
    output_vector = np.asarray([0, 0, 0], dtype=np.float32, order='F')
    # determine the reciprocal space vectors for v1 and v2
    pyEMsoft.crystal.transspace(LatCell, v1, g1, inspace, outspace)
    pyEMsoft.crystal.transspace(LatCell, v2, g2, inspace, outspace)
    # redefine the space of input vector (reciprocal space)
    inspace = 'r'
    # redefine the space of the output vector (direct space)
    outspace = 'd'
    pyEMsoft.crystal.calccross(LatCell, g1, g2, output_vector, inspace, outspace, True)
    print('The output vector cross product is', output_vector, 
    'in the', Tools.get_space_string(outspace))
    pyEMsoft.crystal.calccross(LatCell, g1, g2, output_vector, inspace, inspace, True)
    print('or',output_vector,'in the', Tools.get_space_string(inspace), '\n')


.. [1] Morawiec, A., 2003. Orientations and rotations. Springer-Verlag.
.. [2] Rowenhorst, D., Rollett, A.D., Rohrer, G.S., Groeber, M., Jackson, M., Konijnenberg, P.J. and De Graef, M., 2015. Consistent representations of and conversions between 3D rotations. Modelling and Simulation in Materials Science and Engineering, 23(8), p.083501.
.. [3] De Graef, M., 2003. Introduction to conventional transmission electron microscopy. Cambridge University Press.
.. [4] Callahan, P.G. and De Graef, M., 2013. Dynamical electron backscatter diffraction patterns. Part I: Pattern simulations. Microscopy and Microanalysis, 19(5), pp.1255-1265.