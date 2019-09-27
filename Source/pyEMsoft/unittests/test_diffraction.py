import sys
import os
sys.path.append('../')
import pyEMsoft 
import numpy as np
import unittest


class Test_Diffraction(unittest.TestCase):
    # a crystal unitcell needs to be created before testing the routines
    # define the unitcell using typedefs to store crystallographic data
    LatCell = pyEMsoft.typedefs.unitcell()
    # define gnode class that is computed by the CalcUcg in the diffraction module 
    # details regarding this class can be found in the typedefs.f90 script
    rlp = pyEMsoft.typedefs.gnode()
    # file name of the crystal data file
    LatCell.fname = 'Ni.xtal'
    pyEMsoft.hdfsupport.crystaldata(LatCell)

    def test_01_getVolt(self):
        # get the accelerating voltage, relativistic correction factor
        # relativistic accelerating potential, electron wavelength
        # obtain scattering factor sets, mean inner potential, interaction constant
        # this routine uses many other rountines such as getscatfac, CalcUcg, Calcwavelength
        pyEMsoft.diffraction.getvoltage(self.LatCell, self.rlp, True)

    def test_02_calcdiffangle(self):
        # calculate 2theta diffraction angle for a plane (hkl)
        Angle = pyEMsoft.diffraction.calcdiffangle(self.LatCell, 1, 1, 1)
        print('\nDiffraction angle for (111) is:', Angle, '(rad)\n')


if __name__ == '__main__':
    unittest.main()
