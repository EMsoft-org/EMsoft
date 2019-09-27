# unittest file for quaternion operations
import sys
sys.path.append('../')
import pyEMsoft 
import numpy as np
import unittest
from random import randint 


class Test_QuaternionsMath(unittest.TestCase):
    # define an arbitrary quaternion (single or double)
    q1 = np.asarray([1, 2, 3, 4], dtype=np.float32)
    # define another arbitray quaternion (single or double)
    q2 = np.asarray([1, 2, 3, 2], dtype=np.float32) 

    def setUp(self):
        pass

    def test_01_quaternion_print(self):
        # print q1 quaternion
        print('q1=')
        pyEMsoft.quaternions.quaternion_print(self.q1)
        # print q2 quaternion
        print('q2=')
        pyEMsoft.quaternions.quaternion_print(self.q2)

    def test_02_quaternion_normalization(self):
        # use the EMsoft function to find norm of the quaternion
        q1  = self.q1 / pyEMsoft.quaternions.cabs(self.q1)
        print('nomalized q1:')
        # print normalized quaternion
        pyEMsoft.quaternions.quaternion_print(q1)

    def test_03_quaternion_multiplication(self):
        # multiplication of q1 and q2
        print('q1 * q2=')
        q3 = pyEMsoft.quaternions.quat_mult(self.q1, self.q2)
        pyEMsoft.quaternions.quaternion_print(q3)

    def test_04_quaternion_devision(self):
        # multiplication of q1 and q2
        q3 = pyEMsoft.quaternions.quat_div(self.q1, self.q2)
        print('q1/q2=')
        pyEMsoft.quaternions.quaternion_print(q3)

    def test_05_quaternion_complex_conjuate(self):
        q_conj = pyEMsoft.quaternions.conjg(self.q1)
        print(' conjugate of q1=', q_conj,'\n')

    def test_06_quaternion_inner_product(self):
        q_inner =  pyEMsoft.quaternions.quat_innerproduct(self.q1, self.q2)
        print('Inner product between q1 and q2=', q_inner,'\n')

    def test_07_quaternion_angle(self):
        q1 = self.q1 / pyEMsoft.quaternions.cabs(self.q1)
        q2 = self.q2 / pyEMsoft.quaternions.cabs(self.q2)
        q_angle = pyEMsoft.quaternions.quat_angle(q1, q2)
        print('Angle between q1 and q2=', q_angle, '(rads)\n')

    def test_08_quaternion_rotation(self):
        # define the vector to be rotated
        v = np.asarray([1, 0, 0], dtype=np.float64)
        # normalize the quaternion
        q1 = self.q1 / pyEMsoft.quaternions.cabs(self.q1)
        # apply the quaternion rotation to the defined vector
        q1_rot = pyEMsoft.quaternions.quat_lp(q1, v)
        print('Rotation of', v, 'through quaternion', q1, 'is', q1_rot,'\n')

        # interpolation between two quaternions
    def test_09_quaternion_slerp(self):
        q1 = self.q1 / pyEMsoft.quaternions.cabs(self.q1)
        q2 = self.q2 / pyEMsoft.quaternions.cabs(self.q2)
        n = 10
        q_slerp = pyEMsoft.quaternions.quat_slerp(q1, q2, n)
        print(type(q_slerp))
        print('Interpolation between', q1, 'and', q2, 'is', q_slerp, '\n')

    def test_10_random_quaternion(self):
        # default integer seed vector
        seed = pyEMsoft.rng.rng_t()
        print('The default seed vector:', seed, '\n')
        # seeds the RNG with a single random integer and a default seed vector
        seed_rand_int = randint(1, 100000000)
        pyEMsoft.rng.rng_seed(seed, seed_rand_int)
        print('The new seed vector:', seed, '\n')
        q_rand = pyEMsoft.quaternions.quat_marsaglia(seed)
        print('Random quaternion using the Marsaglia approach', q_rand, '\n', '\n')

if __name__ == '__main__':
    unittest.main()