import sys
sys.path.append('../')
import pyEMsoft 
from pyEMsoftTools import Tools
import numpy as np
import unittest
import math


class Test_EBSDmod(unittest.TestCase):

    def test_readAngle(self):
        # EBSD name list types
        enl = pyEMsoft.namelisttypedefs.EBSDNameListType()
        # define name of the file containing angles (eu or qu)
        enl.anglefile='euler.txt'
        enl.eulerconvention='hkl'
        #enl.anglefiletype = 'orientations'
        angles=pyEMsoft.ebsdmod.EBSDAngleType()
        # verbose=True converts eu to qu, hkl to tsl
        numangles = pyEMsoft.ebsdmod.ebsdreadangles(enl,angles,verbose=True)
        # the quaternions are saved in columns
        print(angles,'\n')

    def test_readEBSDFile(self):
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
        print('Dimension of accum_e',EBSDMCdata.accum_e.shape)
        print('Dimension of mLPNH:', EBSDMPdata.mlpnh.shape)
        print('Dimension of mLPSH:', EBSDMPdata.mlpsh.shape)
        print('Dimension of masterSPNH:', EBSDMPdata.masterspnh.shape)
        print('Dimension of masterSPSH:', EBSDMPdata.masterspsh.shape)
        pyEMsoft.hdfsupport.h5close_emsoft(hdferr)

if __name__ == '__main__':
    unittest.main()
