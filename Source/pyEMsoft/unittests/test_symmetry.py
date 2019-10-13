# unittest file for symmetry related routines
import sys
sys.path.append('../')
import pyEMsoft 
from pyEMsoftTools import Tools
import numpy as np
import unittest


class Test_Symmetry(unittest.TestCase):
    # a crystal unitcell needs to be created before testing the routines
    # define the unitcell using typedefs to store crystallographic data
    LatCell = pyEMsoft.typedefs.unitcell()
    # file name of the crystal data file (in the XtalFolder)
    LatCell.fname = 'Ni.xtal'
    # readin the hdf5 data from XtalFolder using crystaldata function 
    # this function also uses readDataHDF (HDFsupport.f90) and CalcMatrices (crystal.f90)
    pyEMsoft.hdfsupport.crystaldata(LatCell, verbose=True)

    def setUp(self):
        pass

    def test_01_get_Laue_Group_Number(self):
        LG_N = pyEMsoft.symmetry.getlauegroupnumber(self.LatCell.sym_sgnum)
        print('The Laue group number of space group', self.LatCell.sym_sgnum, 'is', LG_N, '\n')

    # determine whether or not a given reflection is absent due to lattice 
    # centering operations.
    def test_02_Is_G_Allowed(self):
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

    # get diffraction group and pattern symmetry
    def test_03_GetDiffraction_Group(self):
        # list the point groups
        print('\n')
        pyEMsoft.symmetry.listpointgroups()
        print('\n')
        # get first space group number for point groups
        SGNUM = self.LatCell.sym_sgnum
        print('Space group of this unit cell is:', SGNUM)
        # use the pyEMsoftTools to determine the point group number from the 
        # space group number
        pgnum = Tools.get_point_group(SGNUM)
        print('The point group of this space group', SGNUM, 'is', pgnum)
        # define the zone axis
        uvw = np.array([1, 0, 0], int)
        # call the getdiffractiongroup function
        diffraction_group_number = pyEMsoft.symmetry.\
        getdiffractiongroup(self.LatCell, uvw, pgnum)
        print('Diffraction group number for point group number', pgnum, 
        'on zone axis', uvw, 'is', diffraction_group_number, '\n')
        # call the getpatternsymmetry function
        diffraction_group_number = pyEMsoft.symmetry.\
        getpatternsymmetry(self.LatCell, uvw, pgnum, True)

    # generate the symmetry matrices for one of the 2D planar point groups
    def test_04_generate_2d_symmetry(self):
        # get the 10 2D point group symbols from the typedefs module
        pgtwd = Tools.get_character_array(pyEMsoft.typedefs.pgtwd)
        print('10 2D point group symbols:\n', pgtwd)
        # select a 2D point group
        pgnum = 5
        # get the 2D symmetry data from typedefs module
        tdpg = pyEMsoft.typedefs.symdata2D()
        # generate the symmetry matrices
        pyEMsoft.symmetry.generate2dsymmetry(tdpg, pgnum)
        print('2D point group selected is:', pgtwd[0][tdpg.sym_pgnum-1])
        print('2D point group order is', tdpg.sym_matnum)
        print('Symmetry matrices are:\n', tdpg.sym_direc)

    def test_05_Calc_Positions(self):
        # Compute all atom positions in the fundamental unit cell ('v')
        pyEMsoft.symmetry.calcpositions(self.LatCell, 'v')
        print('\nNumber of Atom Types', self.LatCell.atom_ntype, '\n')
        print('Atom Type (atomic number):', self.LatCell.atom_type[0:self.LatCell.atom_ntype], '\n')
        print('Fractional coordiantes, Site occupation and Debye-Waller factor: \n', 
        self.LatCell.atom_pos[0:self.LatCell.atom_ntype, 0:5], '\n')
        n_atoms_type = self.LatCell.numat[0:self.LatCell.atom_ntype]
        print(n_atoms_type)
        # get the symbols for elements
        Elements=Tools.get_character_array(pyEMsoft.constants.atom_sym)
        # calculate the position of the atom
        for i in range(n_atoms_type.shape[0]):
            print('Atom positions for atomic number', Elements[0,self.LatCell.atom_type[i]-1], '\n', self.LatCell.apos[i, 0:n_atoms_type[i], 0:3],'\n')
       

if __name__ == '__main__':
    unittest.main()
