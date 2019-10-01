import sys
sys.path.append('../')
import pyEMsoft 
import numpy as np
import unittest


class Test_Lambert(unittest.TestCase):
    
    xy = np.asarray([0.3, 0.2], dtype=np.float32)
    ierr = np.asarray([0], dtype=np.int32, order='F')

    def setUp(self):
        pass

    def test_01_LambertSquareToSphere(self):
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        print('2D square coordinates', self.xy, 'is transformed into 3D coordinates',
        xyz, '\n')

    def test_02_LambertSphereToSqaure(self):
        # the input values must be absolute values 
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        xy = pyEMsoft.lambert.lambertspheretosquare(xyz, self.ierr)
        print('3D coordinates', xyz, 'is transformed into 2D square coordinates',
        xy, '\n')

    def test_03_LambertHexToSqaure(self):
        xyz = pyEMsoft.lambert.lamberthextosphere(self.xy, self.ierr)
        print('2D hexagonal coordinates', self.xy, 'is transformed into 3D coordinates',
        xyz, '\n')

    def test_04_LambertSphereToHex(self):
        xyz = pyEMsoft.lambert.lamberthextosphere(self.xy, self.ierr)
        xy = pyEMsoft.lambert.lambertspheretohex(xyz, self.ierr)
        print('3D coordinates', xyz, 'is transformed into 2D hexagonal coordinates',
        xy, '\n')

    def test_05_GetSextant(self):
        res = pyEMsoft.lambert.getsextantdouble(self.xy)
        print('2D hexagonal coordinates', self.xy, 'belongs to the number', res,
        'sextant', '\n')

    def test_06_LambertCubeToBall(self):
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        xyz_b = pyEMsoft.lambert.lambertcubetoball(xyz, self.ierr)
        print('3D cubic coordinates', xyz, 'is transformed into 3D ball coordinates',
        xyz_b, '\n')

    def test_06_LambertBallToCube(self):
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        xyz_b = pyEMsoft.lambert.lambertcubetoball(xyz,0)
        xyz_c = pyEMsoft.lambert.lambertballtocube(xyz_b,0)
        print('3D ball coordinates', xyz_b, 'is transformed into 3D cubic coordinates',
        xyz_c, '\n')

    def test_07_LambertBallToQuaternion(self):
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        xyz_b = pyEMsoft.lambert.lambertcubetoball(xyz, self.ierr)
        q = pyEMsoft.lambert.lambertballtoquaternion(xyz_b, self.ierr)
        print('3D ball coordinates', xyz_b, 'is transformed into quaternion',
        q, '\n')

    def test_08_LambertQuaternionToBall(self):
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        q = pyEMsoft.lambert.lambertcubetoquaternion(xyz, self.ierr)
        print('3D cubic coordinates', xyz, 'is transformed into quaternion',
        q, '\n')

    def test_09_StereographicForward(self):
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        sg = pyEMsoft.lambert.stereographicforward(xyz,0, radius=None)
        print('3D cubic coordinates', xyz, 'is transformed into stereographic projection\
 coordinates',sg, '\n')

    def test_10_StereographicInverse(self):
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        sg = pyEMsoft.lambert.stereographicforward(xyz, self.ierr, radius=None)
        xyz = pyEMsoft.lambert.stereographicinverse(sg, self.ierr, radius=1, 
        quat = None)
        print('Stereographic projection coordinates', sg, 'is transformed into 3D cubic\
 coordinates', xyz, '\n')

    def test_11_LambertForward(self):
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        lb = pyEMsoft.lambert.lambertforward(xyz, self.ierr, radius=1)
        print('3D Cubic coordinates', xyz, 'is transformed into 2D Lambert\
 coordinates', lb, '\n')

    def test_12_LambertInverse(self):
        xyz = pyEMsoft.lambert.lambertsquaretosphere(self.xy, self.ierr)
        lb = pyEMsoft.lambert.lambertforward(xyz, self.ierr, radius=1)
        xyz = pyEMsoft.lambert.lambertinverse(lb, self.ierr, radius=1)
        print('2D Lambert coordinates', lb, 'is transformed into 3D cubic\
 coordinates', xyz, '\n')

if __name__ == '__main__':
    unittest.main()