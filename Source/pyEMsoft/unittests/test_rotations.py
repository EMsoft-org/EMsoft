# unittest file for all the rotation representations
import sys
sys.path.append('../')
import pyEMsoft 
import numpy as np
import unittest


class Test_Quaternion(unittest.TestCase):
    # define an arbitrary quaternion
    q = np.asarray([1, 2, 3, 4], dtype=np.float64)

    # use the EMsoft function to find norm of the quaternion
    q  = q / pyEMsoft.Quaternions.cabs(q)
        
    def setUp(self):
        print('Quaternion',self.q,' is converted to')

    # quaternion to Euler angles    
    def test_qu2eu(self):
        eu = pyEMsoft.Rotations.qu2eu(self.q)
        print('Euler angles:', eu, '\n')

    # quaternion to axis angle pair
    def test_qu2ax(self):
        ax = pyEMsoft.Rotations.qu2ax(self.q)
        print('axis-angle pair:', ax, '\n')

    # quaternion to orientation matrix
    def test_qu2om(self):
        om = pyEMsoft.Rotations.qu2om(self.q)
        print('orientation matrix:', om, '\n')
    
    # quaternion to homochoric
    def test_qu2ho(self):
        ho = pyEMsoft.Rotations.qu2ho(self.q)
        print('homochoric:', ho, '\n')

    # quaternion to Rodrigues-Frank vector
    def test_qu2ro(self):
        ro = pyEMsoft.Rotations.qu2ro(self.q)
        print('Rodrigues-Frank vector:', ro, '\n')

    # quaternion to cubochoric
    def test_qu2cu(self):
        cu = pyEMsoft.Rotations.qu2cu(self.q)
        print('cubochoric:', cu, '\n')

    # quaternion to stereographic
    def test_qu2st(self):
        st = pyEMsoft.Rotations.qu2st(self.q)
        print('stereographic:', st, '\n')


class Test_Euler(unittest.TestCase):
    # define Euler angle from the Test_Quaternion
    eu = pyEMsoft.Rotations.qu2eu(Test_Quaternion.q)
    
    def setUp(self):
        print('Euler angles',self.eu,' is converted to')
        
    # Euler angles to orientation matrix   
    def test_eu2om(self):
        om = pyEMsoft.Rotations.eu2om(self.eu)
        print('orientation matrix:', om, '\n')

    # Euler angles to axis angle pair 
    def test_eu2ax(self):
        ax = pyEMsoft.Rotations.eu2ax(self.eu)
        print('axis-angle pair:', ax, '\n')

    # Euler angles to quaternion   
    def test_eu2qu(self):
        qu = pyEMsoft.Rotations.eu2qu(self.eu)
        print('quaternion:', qu, '\n')

    # Euler angles to homochoric
    def test_eu2ho(self):
        ho = pyEMsoft.Rotations.eu2ho(self.eu)
        print('homochoric:', ho, '\n')

    # Euler angles to Rodrigues-Frank vector
    def test_eu2ro(self):
        ro = pyEMsoft.Rotations.eu2ro(self.eu)
        print('Rodrigues-Frank vector:', ro, '\n')

    # Euler angles to cubochoric
    def test_eu2cu(self):
        cu = pyEMsoft.Rotations.eu2cu(self.eu)
        print('cubochoric:', cu, '\n')

    # Euler angles to stereographic
    def test_eu2st(self):
        st = pyEMsoft.Rotations.eu2st(self.eu)
        print('stereographic:', st, '\n')


class Test_Axis_Angle(unittest.TestCase):
    # define axis angle pair from the Test_Quaternion
    ax = pyEMsoft.Rotations.qu2ax(Test_Quaternion.q)
    
    def setUp(self):
        print('Axis-angle pair', self.ax,' is converted to')
        
    # Axis angle pair to orientation matrix   
    def test_ax2om(self):
        om = pyEMsoft.Rotations.ax2om(self.ax)
        print('orientation matrix:', om, '\n')

    # Axis angle pair to Euler angles
    def test_ax2eu(self):
        eu = pyEMsoft.Rotations.ax2eu(self.ax)
        print('Euler angles:', eu, '\n')

    # Axis angle pair to quaternion   
    def test_ax2qu(self):
        qu = pyEMsoft.Rotations.ax2qu(self.ax)
        print('quaternion:', qu, '\n')

    # Axis angle pair to homochoric
    def test_ax2ho(self):
        ho = pyEMsoft.Rotations.ax2ho(self.ax)
        print('homochoric:', ho, '\n')

    # Axis angle pair to Rodrigues-Frank vector
    def test_ax2ro(self):
        ro = pyEMsoft.Rotations.ax2ro(self.ax)
        print('Rodrigues-Frank vector:', ro, '\n')

    # Axis angle pair to cubochoric
    def test_ax2cu(self):
        cu = pyEMsoft.Rotations.ax2cu(self.ax)
        print('cubochoric:', cu, '\n')

    # Axis angle pair to stereographic
    def test_ax2st(self):
        st = pyEMsoft.Rotations.ax2st(self.ax)
        print('stereographic:', st, '\n')   


class Test_Orientation_Matrix(unittest.TestCase):
    # define axis angle pair from the Test_Quaternion
    om = pyEMsoft.Rotations.qu2om(Test_Quaternion.q)
    
    def setUp(self):
        print('Orientation matrix', self.om,' is converted to')
        
    # orientation matrix to axis angle pair 
    def test_om2ax(self):
        ax = pyEMsoft.Rotations.om2ax(self.om)
        print('axis-angle pair:', ax, '\n')

    # orientation matrix to Euler angles
    def test_om2eu(self):
        eu = pyEMsoft.Rotations.om2eu(self.om)
        print('Euler angles:', eu, '\n')

    # orientation matrix to quaternion   
    def test_om2qu(self):
        qu = pyEMsoft.Rotations.om2qu(self.om)
        print('quaternion:', qu, '\n')

    # orientation matrix to homochoric
    def test_om2ho(self):
        ho = pyEMsoft.Rotations.om2ho(self.om)
        print('homochoric:', ho, '\n')

    # orientation matrix to Rodrigues-Frank vector
    def test_om2ro(self):
        ro = pyEMsoft.Rotations.om2ro(self.om)
        print('Rodrigues-Frank vector:', ro, '\n')

    # orientation matrix to cubochoric
    def test_om2cu(self):
        cu = pyEMsoft.Rotations.om2cu(self.om)
        print('cubochoric:', cu, '\n')

    # orientation matrix to stereographic
    def test_om2st(self):
        st = pyEMsoft.Rotations.om2st(self.om)
        print('stereographic:', st, '\n')   


class Test_Homochoric(unittest.TestCase):
    # define homochoric from the Test_Quaternion
    ho = pyEMsoft.Rotations.qu2ho(Test_Quaternion.q)
    
    def setUp(self):
        print('Homochoric', self.ho,' is converted to')
        
    # homochoric to axis angle pair 
    def test_ho2ax(self):
        ax = pyEMsoft.Rotations.ho2ax(self.ho)
        print('axis-angle pair:', ax, '\n')

    # homochoric to Euler angles
    def test_ho2eu(self):
        eu = pyEMsoft.Rotations.ho2eu(self.ho)
        print('Euler angles:', eu, '\n')

    # homochoric to quaternion   
    def test_ho2qu(self):
        qu = pyEMsoft.Rotations.ho2qu(self.ho)
        print('quaternion:', qu, '\n')

    # homochoric to orientation matrix
    def test_ho2om(self):
        om = pyEMsoft.Rotations.ho2om(self.ho)
        print('homochoric:', om, '\n')

    # homochoric to Rodrigues-Frank vector
    def test_ho2ro(self):
        ro = pyEMsoft.Rotations.ho2ro(self.ho)
        print('Rodrigues-Frank vector:', ro, '\n')

    # homochoric to cubochoric
    def test_ho2cu(self):
        cu = pyEMsoft.Rotations.ho2cu(self.ho)
        print('cubochoric:', cu, '\n')

    # homochoric to stereographic
    def test_ho2st(self):
        st = pyEMsoft.Rotations.ho2st(self.ho)
        print('stereographic:', st, '\n')   


class Test_Cubochoric(unittest.TestCase):
    # define cubochoric from the Test_Quaternion
    cu = pyEMsoft.Rotations.qu2cu(Test_Quaternion.q)
    
    def setUp(self):
        print('Cubochoric', self.cu,' is converted to')
        
    # cubochoric to axis angle pair 
    def test_cu2ax(self):
        ax = pyEMsoft.Rotations.cu2ax(self.cu)
        print('axis-angle pair:', ax, '\n')

    # cubochoric to Euler angles
    def test_cu2eu(self):
        eu = pyEMsoft.Rotations.cu2eu(self.cu)
        print('Euler angles:', eu, '\n')

    # cubochoric to quaternion   
    def test_cu2qu(self):
        qu = pyEMsoft.Rotations.cu2qu(self.cu)
        print('quaternion:', qu, '\n')

    # cubochoric to orientation matrix
    def test_cu2om(self):
        om = pyEMsoft.Rotations.cu2om(self.cu)
        print('orientation matrix:', om, '\n')

    # cubochoric to Rodrigues-Frank vector
    def test_cu2ro(self):
        ro = pyEMsoft.Rotations.cu2ro(self.cu)
        print('Rodrigues-Frank vector:', ro, '\n')

    # cubochoric to homochoric
    def test_cu2ho(self):
        ho = pyEMsoft.Rotations.cu2ho(self.cu)
        print('homochoric:', ho, '\n')

    # homochoric to stereographic
    def test_cu2st(self):
        st = pyEMsoft.Rotations.cu2st(self.cu)
        print('stereographic:', st, '\n') 


class Test_Stereographic(unittest.TestCase):
    # define stereographic from the Test_Quaternion
    st = pyEMsoft.Rotations.qu2st(Test_Quaternion.q)
    
    def setUp(self):
        print('Stereographic', self.st,' is converted to')
        
    # stereographic to axis angle pair 
    def test_st2ax(self):
        ax = pyEMsoft.Rotations.cu2ax(self.st)
        print('axis-angle pair:', ax, '\n')

    # stereographic to Euler angles
    def test_st2eu(self):
        eu = pyEMsoft.Rotations.st2eu(self.st)
        print('Euler angles:', eu, '\n')

    # stereographic to quaternion   
    def test_st2qu(self):
        qu = pyEMsoft.Rotations.st2qu(self.st)
        print('quaternion:', qu, '\n')

    # stereographic to orientation matrix
    def test_st2om(self):
        om = pyEMsoft.Rotations.st2om(self.st)
        print('homochoric:', om, '\n')

    # stereographic to Rodrigues-Frank vector
    def test_st2ro(self):
        ro = pyEMsoft.Rotations.st2ro(self.st)
        print('Rodrigues-Frank vector:', ro, '\n')

    # stereographic to homochoric
    def test_st2ho(self):
        ho = pyEMsoft.Rotations.st2ho(self.st)
        print('homochoric:', ho, '\n')

    # stereographic to cubochoric
    def test_st2cu(self):
        st = pyEMsoft.Rotations.st2cu(self.st)
        print('cubochoric:', st, '\n') 



class Test_Rodrigues_Frank_Vector(unittest.TestCase):
    # define Rodrigues-Frank vectorfrom the Test_Quaternion
    ro = pyEMsoft.Rotations.qu2ro(Test_Quaternion.q)
    
    def setUp(self):
        print('Rodrigues-Frank vector', self.ro,' is converted to')
        
    # Rodrigues-Frank vector to axis angle pair 
    def test_ro2ax(self):
        ax = pyEMsoft.Rotations.ro2ax(self.ro)
        print('axis-angle pair:', ax, '\n')

    # Rodrigues-Frank vector to Euler angles
    def test_ro2eu(self):
        eu = pyEMsoft.Rotations.ro2eu(self.ro)
        print('Euler angles:', eu, '\n')

    # Rodrigues-Frank vector to quaternion   
    def test_ro2qu(self):
        qu = pyEMsoft.Rotations.ro2qu(self.ro)
        print('quaternion:', qu, '\n')

    # Rodrigues-Frank vector to orientation matrix
    def test_ro2om(self):
        om = pyEMsoft.Rotations.ro2om(self.ro)
        print('homochoric:', om, '\n')

    # Rodrigues-Frank vector to stereographic 
    def test_ro2st(self):
        st = pyEMsoft.Rotations.ro2st(self.ro)
        print('stereographic :', st, '\n')

    # Rodrigues-Frank vector to homochoric
    def test_ro2ho(self):
        ho = pyEMsoft.Rotations.ro2ho(self.ro)
        print('homochoric:', ho, '\n')

    # Rodrigues-Frank vector to cubochoric
    def test_ro2cu(self):
        st = pyEMsoft.Rotations.ro2cu(self.ro)
        print('cubochoric:', st, '\n') 

if __name__ == '__main__':
    unittest.main()