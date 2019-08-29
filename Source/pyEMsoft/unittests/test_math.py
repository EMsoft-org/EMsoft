# unittest file for all the rotation representations
import sys
sys.path.append('../')
import pyEMsoft 
import numpy as np
import unittest


class Test_Math(unittest.TestCase):

    def setUp(self):
        pass

    def test_minvert(self):
        # define an arbitrary quaternion
        q = np.asarray([1, 2, 3, 4], dtype=np.float64)
        # use the EMsoft function to find norm of the quaternion
        q  = q / pyEMsoft.Quaternions.cabs(q)
        om = pyEMsoft.Rotations.qu2om(q)
        print('Orientation matrix:', om, '\n')
        # there are some special cases where a matrix needs to be allocated in python
        # but the results are filled in the fortran code. once such example is the 
        # mInvert subroutine in math module. here we'll just invert the 'om' array
        # calculated above.
        om_invert = np.asfortranarray(np.zeros([3, 3]), dtype=np.float32)
        # notice how we have to specify that the array is fortran ordering as the 
        # default ordering in python is c. this can be seen in the flags field on om_invert
        # print('\n om FLAGS:\n', om_invert.flags, '\n')
        # actually call the routine now. the True flag is used to tell the routine that the
        # matrix I'm passing is unitary. 
        pyEMsoft.Math.minvert(om, om_invert, True)
        print('\n om inverse:', om_invert, '\n')
    
    def test_polardecomposition(self):
        F=np.array([[1, 0.495, 0.5],[-0.333,1,-0.247],[0.959,0,1.5]], dtype=np.double)
        Rmatrix= np.asfortranarray(np.zeros([3, 3]), dtype=np.double)
        Smatrix= np.asfortranarray(np.zeros([3, 3]), dtype=np.double)
        pyEMsoft.Math.getpolardecomposition(F, Rmatrix, Smatrix)
        print('Polar decomposition of:\n',F, '\n\ngives rotation matrix\n',Rmatrix,'\n\nand stretch matrix\n',Smatrix)
        #print('\n Rmatrix FLAGS:\n', Rmatrix.flags, '\n')
        #print('\n Smatrix FLAGS:\n', Smatrix.flags, '\n')
    
    def test_cross3(self):
        u=np.array([1,0,0], dtype=np.float32)
        v=np.array([0,1,0], dtype=np.float32)
        w=pyEMsoft.Math.cross3(u,v)
        print('cross product of',u,'and',v,'is',w,'\n')
    
    def test_get_bit_parameters(self):
        bd ='10int'
        numbits,bitrange,bitmode=pyEMsoft.Math.get_bit_parameters(bd)
        print('number of bits=',numbits,'; bit range=0 to',bitrange,'; bit mode=',bitmode,'\n')

    def test_infty(self):
        infinity=pyEMsoft.Math.infty()
        print('define the value of infinity',infinity,'\n')

    def test_calcdeterminant(self):
        A=np.array([[0, -1, 0],[1,0,0],[0,0,1]], dtype=np.float32)
        determinant=pyEMsoft.Math.calcdeterminant(A,3,3)
        print('\nThe determinant of a rotation matrix is ', determinant,'\n')

    def test_vectormatch(self):
        n=np.array([0,1,2], dtype=np.int)
        v=np.array([1,0,1], dtype=np.int)
        nce=pyEMsoft.Math.vectormatch(3,n,v)
        print('\n the number of common entries in two vectors',n,'and',v,'is',nce,'\n')
    # having some issues with three integer roots
    def test_cubicroots(self):
        coefficients=np.array([1,-7,11,-20], dtype=np.double)
        roots= np.asfortranarray(np.zeros([1,3]), dtype=np.complex)
        pyEMsoft.Math.cubicroots(coefficients,roots)
        print(roots)

if __name__ == '__main__':
    unittest.main()