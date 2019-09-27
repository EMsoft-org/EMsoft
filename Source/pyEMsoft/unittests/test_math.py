# unittest file for all the rotation representations
import sys
sys.path.append('../')
import pyEMsoft 
import numpy as np
import unittest
from random import randint 


class Test_Math(unittest.TestCase):

    def setUp(self):
        pass

    def test_01_minvert(self):
        # define an arbitrary quaternion
        q = np.asarray([1, 2, 3, 4], dtype=np.float32)
        # convert the quaternion to orientaiotn matrix
        q  = q / pyEMsoft.quaternions.cabs(q)
        om = pyEMsoft.rotations.qu2om(q)
        print('\nOrientation matrix:', om, '\n')
        # preallocate the output with numpy with Fortran order in memory
        om_invert = np.asarray(np.zeros([3, 3]), dtype=np.float32, order='F')
        # notice how we have to specify that the array is fortran ordering as 
        # the default ordering in python is c. 
        # this can be seen in the flags field on om_invert
        print('\n om_invert FLAGS:\n', om_invert.flags, '\n')
        # true flag indicates that the input matrix is unitary
        pyEMsoft.math.minvert(om, om_invert, True)
        print('om inverse:', om_invert, '\n')

    def test_02_polardecomposition(self):
        # define an input deformation gradient tensor
        F = np.array([[1, 0.495, 0.5], [-0.333, 1, -0.247], [0.959, 0, 1.5]],
        dtype=np.float64)
        # preallocate the output matrices with Fortran order in memory
        Rmatrix = np.asarray(np.zeros([3, 3]), dtype=np.float64, order='F')
        Smatrix = np.asarray(np.zeros([3, 3]), dtype=np.float64, order='F')
        # call the polardecomposition routine (relies on the lapack library)
        pyEMsoft.math.getpolardecomposition(F, Rmatrix, Smatrix)
        print('Polar decomposition of:\n', F, '\ngives rotation matrix\n',
        Rmatrix, '\nand stretch matrix\n', Smatrix, '\n')
        print('Rmatrix FLAGS:\n', Rmatrix.flags, '\n')
        print('Smatrix FLAGS:\n', Smatrix.flags, '\n')

    def test_03_cross3(self):
        # define two random vectors
        u = np.array([1, 0, 0], dtype=np.float32)
        v = np.array([0, 1, 0], dtype=np.float32)
        # fine the cross product of the two vectors
        w = pyEMsoft.math.cross3(u, v)
        print('Cross product of', u, 'and', v, 'is', w, '\n')

    def test_04_get_bit_parameters(self):
        # define the bit parameters e.g. 10int
        bd = '10int'
        # then call the routine to determine the number of bits, bitrange, and bit mode
        numbits, bitrange, bitmode = pyEMsoft.math.get_bit_parameters(bd)
        print('Number of bits=', numbits, '; bit range=0 to', bitrange, 
        '; bit mode=', bitmode.decode('utf-8'), '\n')

    def test_05_infty(self):
        # get the value of infinity
        infinity = pyEMsoft.math.infty()
        print('Define the value of infinity:', infinity, '\n')

    def test_06_calcdeterminant(self):
        # define a rotation matrix (determinant of it should be 1)
        A = np.array([[0, -1, 0], [1, 0, 0], [0, 0, 1]], dtype=np.float32)
        # call the routine to find the determinant of the matrix
        determinant = pyEMsoft.math.calcdeterminant(A, 3, 3)
        print('The determinant of a rotation matrix is ', determinant, '\n')

    def test_07_vectormatch(self):
        # define two random vectors
        n = np.array([0, 1, 2], dtype=np.int)
        v = np.array([1, 0, 1], dtype=np.int)
        # call the routine to find out the common entries of the two vectors
        nce = pyEMsoft.math.vectormatch(3, n, v)
        print('The number of common entries in two vectors', n, 'and', v, 'is', nce, '\n')

    def test_08_cubicroots(self):
        # define the coeffcients of the cubic equation
        coefficients = np.asarray([5, -8, 11, -20])
        # define the output roots in Fortran order (complex128)
        roots = np.asarray(np.zeros([1, 3]), dtype=np.complex128, order='F')
        # find the roots
        pyEMsoft.math.cubicroots(coefficients, roots)
        print('The roots are:', roots)

if __name__ == '__main__':
    unittest.main()