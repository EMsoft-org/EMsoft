# unittest file for crystallography related routines
import sys
sys.path.append('../')
import pyEMsoft 
from pyEMsoftTools import Tools
import numpy as np
import unittest


class Test_Crystal(unittest.TestCase):
    # a crystal unitcell needs to be created before testing the routines
    # define the unitcell using typedefs to store crystallographic data
    LatCell = pyEMsoft.typedefs.unitcell()
    # file name of the crystal data file (in the XtalFolder)
    LatCell.fname = 'Ni.xtal'
    # readin the hdf5 data from XtalFolder using crystaldata function 
    # this function also uses readDataHDF (HDFsupport.f90) and CalcMatrices (crystal.f90)
    pyEMsoft.hdfsupport.crystaldata(LatCell, verbose=True)
 
    # define an arbitrary quaternion
    q = np.asarray([1, 2, 3, 4], dtype=np.float32)
    # normalization of quaternion
    q = q / pyEMsoft.quaternions.cabs(q)

    def test_01_GetEMsoftXtalSystem(self):
        # convert space group to crystal system
        XS = pyEMsoft.crystal.getemsoftxtalsystem(225)
        print('Crystal system of space group 225 is', XS,'\n')

    def test_02_CalcMatrices(self):
        # print the output LatCell
        print('Direct metric tensor is:\n', self.LatCell.dmt, '\n')
        print('Reciprocal metric tensor is:\n', self.LatCell.rmt, '\n')
        print('Direct structure matrix is:\n', self.LatCell.dsm, '\n')
        print('Reciprocal structure matrix is:\n', self.LatCell.rsm, '\n')
        # if we have the trigonal/rhombohedral case, we need a second direct structure matrix
        if self.LatCell.xtal_system == 5:
            print('Second direct structure matrix is:\n', self.LatCell.trigmat, '\n')

    def test_03_TransSpace(self):
        # define an arbitrary input vector in the direct space
        input_vector = np.asarray([1, 1, 1], dtype=np.float32)
        # define the output array first in fortran order in memory
        output_vector = np.asarray([0, 0, 0], dtype=np.float32, order='F')
        # define the space of input vector (direct space)
        inspace = 'd'
        # define hte space of the output vector (standard cartesian reference frame)
        outspace = 'c'
        # now call the transspace from crystal module to convert the input vector into another space
        pyEMsoft.crystal.transspace(self.LatCell, input_vector, output_vector, inspace, outspace)
        print('The', input_vector, 'in the ', Tools.get_space_string(inspace), 'has been converted to', output_vector,'in', Tools.get_space_string(outspace), '\n')
        # vector components involving a transformation matrix 
        # define the output array first in fortran order in memory (double precision)
        output_vector_t = np.asarray([0, 0, 0], dtype=np.float64, order='F')
        # the transformation here is defined from a random quaternion (ideally this should be matrix with directional cosines formed by the basis vectors of 
        # two coordinates systems (old and new)
        trans_m = pyEMsoft.rotations.qu2om(self.q)
        # call the transcoor function for the coordinate transformation in a defined space ('on'=old to new, 'no'=new to old)
        pyEMsoft.crystal.transcoor(self.LatCell, output_vector, output_vector_t, trans_m, 'c', 'on')
        print('The output vector is', output_vector_t, 'under the transformation matrix\n', trans_m, '\n')

    def test_04_CalDot(self):
        # define two random vectors in the direct space
        input_vector1 = np.asarray([1, 1, 1], dtype=np.float64)
        input_vector2 = np.asarray([1, 1, 0], dtype=np.float64)
        # define the direct space as 'd'
        space = 'd'
        # calculate the dot product between the two vectors in the given space
        dot_product = pyEMsoft.crystal.calcdot(self.LatCell, input_vector1, input_vector2, space)
        print('The doct product betweeen', input_vector1, 'and', input_vector2, 'is', dot_product, 'in the', Tools.get_space_string(space), '\n')

    def test_05_NormVec(self):
        # define an arbitrary input vector in the direct space (single precision)
        input_vector_s = np.asarray([1, 1, 1], dtype=np.float32, order='F')
        # define an arbitrary input vector in the direct space (double precision)
        input_vector_d = np.asarray([1, 1, 1], dtype=np.float64, order='F')
        space = 'd'
        # call normaliztion routines 
        pyEMsoft.crystal._normvecsingle(self.LatCell, input_vector_s, space)
        pyEMsoft.crystal._normvecdouble(self.LatCell, input_vector_d, space)
        # print the output and determine the number of bytes
        print('Total # of bytes in the single precision vector:', input_vector_s.nbytes, '\n')
        print('Total # of bytes in the double precision vector:', input_vector_d.nbytes, '\n')

    def test_06_CalcLength(self):
        # define an arbitrary input vector in the direct space (single precision)
        input_vector_s = np.asarray([1, 1, 1], dtype=np.float32, order='F')
        # define an arbitrary input vector in the direct space (double precision)
        input_vector_d = np.asarray([1, 1, 1], dtype=np.float64, order='F')
        space = 'd'
        # call normaliztion routines 
        length_s = pyEMsoft.crystal._calclengthsingle(self.LatCell, input_vector_s, space)
        length_d = pyEMsoft.crystal._calclengthdouble(self.LatCell, input_vector_d, space)
        # print the output and determine the number of bytes (sys.getsizeof will always output 24 bytes for 
        # both single and double precision)
        print('Total # of bytes in the single precision length:', sys.getsizeof(length_s))
        print('Total # of bytes in the double precision length:', sys.getsizeof(length_d), '\n')

    def test_07_CalcAngle(self):
        # define an arbitrary input vector in the direct space (single precision)
        input_vector1 = np.asarray([1, 1, 1], dtype=np.float32)
        # define an arbitrary input vector in the direct space (double precision)
        input_vector2 = np.asarray([1, 1, 0], dtype=np.float32)
        space = 'd'
        # call calangle routine
        a = pyEMsoft.crystal.calcangle(self.LatCell, input_vector1, input_vector2, space)
        # print the output and determine the number of bytes
        print('The angle between', input_vector1, 'and', input_vector2,'is', a, '(rad)\n')

    def test_08_CalCross(self):
        # define an arbitrary input vector in the direct space (single precision)
        input_vector1 = np.asarray([1, 1, 1], dtype=np.float32)
        # define an arbitrary input vector in the direct space (double precision)
        input_vector2 = np.asarray([1, 1, 0], dtype=np.float32)
        # define the output array first in fortran order in memory (double precision)
        output_vector = np.asarray([0, 0, 0], dtype=np.float32, order='F')
        # define the space of input vector (direct space)
        inspace = 'd'
        # define hte space of the output vector (standard cartesian reference frame)
        outspace = 'c'
        pyEMsoft.crystal.calccross(self.LatCell, input_vector1, input_vector2, output_vector, inspace,outspace,True)
        print('The output vector cross product is', output_vector, 'in the',Tools.get_space_string(outspace),'\n')

    def test_09_Miller_Bravais(self):
        # first we do a Miller to Miller-Bravais indices conversion (switch:'34')
        Miller_index = np.asarray([1, 0, 1], dtype=np.int32)
        Miller_Bravais_index = np.asarray([0, 0, 0, 0], dtype=np.int32, order='F')
        pyEMsoft.crystal.milbrav(Miller_index, Miller_Bravais_index, '34')
        print('Miller indices', Miller_index, 'is converted to Miller-Bravais indices:', Miller_Bravais_index,'\n')
        # then we do a Miller-Bravais to Miller indices conversion (switch:'43')
        Miller_index = np.asarray([0, 0, 0], dtype=np.int32, order='F')
        pyEMsoft.crystal.milbrav(Miller_index, Miller_Bravais_index, '43')
        print('Miller-Bravais indices', Miller_Bravais_index, 'is converted to Miller indices:', Miller_index,'\n')

    def test_10_CalcDensity(self):
        # calculate positions of atoms in the unit cell
        pyEMsoft.symmetry.calcpositions(self.LatCell, 'v')
        # calculate density, average atomic number, average atomic weight
        density, avZ, avA = pyEMsoft.crystal.calcdensity(self.LatCell)
        print(density, avZ, avA)
        print('Density=', density, '(g/cm^3)', 'average atomic number=', avZ, 'average atomic weight=', avA, '(g/mol)\n')
        print('unit cell volume', self.LatCell.vol)
        
if __name__ == '__main__':
    unittest.main()
