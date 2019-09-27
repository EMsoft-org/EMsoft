# unittest file for all the rotation representations
import sys
sys.path.append('../')
import pyEMsoft 
import numpy as np
import unittest
import math


class Test_Others(unittest.TestCase):
    # define an arbitrary quaternion
    q = np.asarray([1, 2, 3, 4], dtype=np.float64)
    # normalization of quaternion
    q = q / pyEMsoft.quaternions._quat_norm_d(q)

    def setUp(self):
        pass

    def test_01_qu_check(self):
        print('q is', self.q.dtype)
        print('norm of double precision quaternion:', math.sqrt(np.sum(np.multiply(self.q, self.q))), '\n')
        # use the _qu_check_d to check if it has positive scalar part and if the norm is unity
        res = pyEMsoft.rotations._qu_check_d(self.q)
        #st=pyEMsoft.rotations._qu2st(self.q)
        #print('norm of double precision quaternion:', math.sqrt(np.sum(np.multiply(st, st))))
        q1 = np.asarray([1, 2, 3, 4], dtype=np.float32)
        # normalization of quaternion
        q1 = q1 / pyEMsoft.quaternions._quat_norm(q1)
        print('norm of single precision quaternion:', math.sqrt(np.sum(np.multiply(q1, q1))), '\n')
        # use the qu_check (or _qu_check) to check if it has positive scalar part and if the norm is unity
        res = pyEMsoft.rotations.qu_check(q1) 

    def test_02_st_check(self):
        st=pyEMsoft.rotations.qu2st(self.q)
        # need to be rescaled to have unity as norm
        st=st*(1/(sum(st*st)**0.5))
        print('norm of single precision stereographic vector:', math.sqrt(np.sum(np.multiply(st, st))), '\n')

    def test_03_init_orientaiton(self):
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
                    res = pyEMsoft.rotations._init_orientation_om_d(inputtype,i, rotcheck=True)
                    print(i, 'to other types\n',res,'\n')
                else:
                    res = pyEMsoft.rotations._init_orientation_d(inputtype, i, rotcheck=False)
                    print(i, 'to other types\n', res, '\n')

    def test_04_RotVec_om(self):
        # define a vector to be transformed
        v = np.array([1, 1, 1])
        # quaternion to orientation matrix conversion
        # using quaterion to do rotation is also possible (quaterion.f90, quart_Lp) 
        om = pyEMsoft.rotations.qu2om(self.q)
        # use a character to switch between active (a) and passive (p) rotations
        v_r = pyEMsoft.rotations.rotatevector(v,om, 'a')
        print('v=', v, 'rotated by:\n', om, '\nbecomes=', v_r, '\n')

    def test_05_RotTensor(self):
        # define a tensor to be transformed
        tensor = np.asarray([[1, 2, 3], [3, 4, 5], [5, 6, 7]])
        # quaternion to orientation matrix conversion
        # using quaterion to do rotation is also possible (quaterion.f90, quart_Lp) 
        om = pyEMsoft.rotations.qu2om(self.q)
        # use a character to switch between active (a) and passive (p) rotations
        tensor_r = pyEMsoft.rotations.rotatetensor2(tensor, om,'p')
        print('tensor=', tensor, 'rotated by:\n', om,'\nbecomes=', tensor_r, '\n')

    def test_06_quat_average(self):
        # define two other arbitrary quaternions
        q1 = np.asarray([1, 2, 3, 3], dtype=np.float64)
        q1 = q1 / pyEMsoft.quaternions._quat_norm_d(q1)
        q2 = np.asarray([1, 2, 3, 5], dtype=np.float64)
        q2 = q2 / pyEMsoft.quaternions._quat_norm_d(q2)
        qlist = np.column_stack((self.q,q1,q2))
        # output standard deviation quaternion
        qstdev = np.asarray(np.zeros(4), dtype=np.float64, order='F')
        # compute the geometrical mean of a list of quaternions 
        res = pyEMsoft.rotations._quat_average_d(qlist, 3, qstdev)
        print('Geometrical mean of the quaternion list:\n', qlist, '\nis:', res, 
        '\nwith standard deviation quaternion:', qstdev, '\n')

    def test_07_lapack(self):
         # define an arbitrary quaternion (single and double precision)
        qs = np.asarray([1, 2, 3, 4], dtype=np.float32)
        qb = np.asarray([1, 2, 3, 4], dtype=np.float64)
        # normalization of quaternion (single and double precision)
        q1 = qs / pyEMsoft.quaternions._quat_norm(qs)
        q2 = qb / pyEMsoft.quaternions._quat_norm_d(qb)
        print(q1,q1.dtype, '\n')
        print(q2,q2.dtype, '\n')
        # quaternion to orientation matrix (single and double precision)
        om1=pyEMsoft.rotations._qu2om(q1)
        om2=pyEMsoft.rotations._qu2om_d(q2)
        # find the determinant
        d_om1=np.linalg.det(om1)
        d_om2=np.linalg.det(om2)
        print(om1, om1.dtype,'determinant of om', d_om1, '\n')
        print(om2, om2.dtype,'determinant of om', d_om2, '\n')
        # orientation matrix to axis angle pair conversion (single and double precision)
        ax1=pyEMsoft.rotations._om2ax(om1)
        print(ax1, ax1.dtype, '\n')
        ax2=pyEMsoft.rotations._om2ax_d(om2)
        print(ax2, ax2.dtype, '\n')

if __name__ == '__main__':
    unittest.main()