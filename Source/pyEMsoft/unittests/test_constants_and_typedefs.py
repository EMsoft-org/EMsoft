# unittest file for all the rotation representations
import sys
sys.path.append('../')
import pyEMsoft 
from pyEMsoftTools import Tools
import numpy as np
import unittest


class Test_Constants(unittest.TestCase):

    def setUp(self):
        pass

    # list a few constants the complete list of constants also include: permeability/permittivity
    # of vacuum, electron charge/rest mass,electron volt, Avogadro's constant, Bohr magnetron
    def test_SIconstants(self):
        print('Value of \u03C0 is %.19f' % (pyEMsoft.constants.cpi), '\n')
        print('Speed of light (c) is %.1f (m/s)' % (pyEMsoft.constants.clight), '\n')
        print('Planck constant (h) is %e (Js)' % (pyEMsoft.constants.cplanck), '\n')
        print('Boltzmann Constant (k) is %e (mˆ2kgsˆ(-1)Kˆ(-1))' % (pyEMsoft.constants.cboltzmann), '\n')

    # list of element symbols and atom color (the original output character array is an numpy.array in ASCII encoded format)
    # a tool is created to get the character array (Tools.get_character_array)
    def test_characterarray(self):
        print('Element Symbols:\n', Tools.get_character_array(pyEMsoft.constants.atom_sym), '\n')
        print('Atom colors for PostScript drawings:\n', Tools.get_character_array(pyEMsoft.constants.atom_color), '\n')

    # print out some numerical arrays
    def test_array(self):
        print('Shannon-Prewitt ionic radii in nanometer:\n', pyEMsoft.constants.atom_spradii, '\n')
        print('Atomic weights for things like density computations (from NIST elemental data base):\n', pyEMsoft.constants.atom_weights, '\n')
        print('Fundamental zone type:\n', pyEMsoft.constants.fztarray, '\n')
        print('Butterfly9x9 Filter:\n', pyEMsoft.constants.butterfly9x9, '\n')


class Test_Typedefs(unittest.TestCase):

    def setUp(self):
        pass

    # print out a lot of useful crystallographic data
    def test_SG(self):
        print('All space group names:\n', Tools.get_character_array(pyEMsoft.typedefs.sym_sgname),'\n')
        print('Extended Hermann-Mauguin symbols for the orthorhombic space groups:\n',Tools.get_character_array(pyEMsoft.typedefs.extendedhmorthsymbols),'\n')
        print('First space group of each crystal system:\n', pyEMsoft.typedefs.sgxsym,'\n')
        print('First space group # for a given point group:\n', pyEMsoft.typedefs.sgpg,'\n')
        print('Numbers of all the symmorphic space groups:\n', pyEMsoft.typedefs.sgsym,'\n')
        print('Number of the symmorphic space group with the same point group symmetry:\n', pyEMsoft.typedefs.sgsymnum,'\n')
        print('10 2D point group symbols:\n', Tools.get_character_array(pyEMsoft.typedefs.pgtwd), '\n')
        print('Inverse table for 2D point groups:\n', pyEMsoft.typedefs.pgtwdinverse, '\n')
        print('2D point group orders :\n', pyEMsoft.typedefs.pgtwdorder, '\n')
        print('32 3D point group symbols:\n', Tools.get_character_array(pyEMsoft.typedefs.pgthd), '\n')

if __name__ == '__main__':
    unittest.main()