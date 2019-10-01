# unittest file for sampling of rotation space SO(3)
import sys
sys.path.append('../')
import pyEMsoft 
import numpy as np
import unittest
from random import randint 


class Test_SO3(unittest.TestCase):

    def setUp(self):
        pass
        
    def test_01_IsinsideFZ(self):
        # default integer seed vector
        seed =pyEMsoft.rng.rng_t()
        print('The default seed vector:', seed,'\n')
        # seeds the RNG with a single random integer and a default seed vector
        seed_rand_int = randint(1, 100000000)
        pyEMsoft.rng.rng_seed(seed, seed_rand_int)
        print('The new seed vector:', seed, '\n')
        q_rand = pyEMsoft.quaternions.quat_marsaglia(seed)
        print('Random quaternion using the Marsaglia approach', q_rand, '\n', '\n')
        # quaternion to Rodrigues coordinates conversion
        rod = pyEMsoft.rotations.qu2ro(q_rand)
        # now pick the point group
        pyEMsoft.symmetry.listpointgroups()
        point_group_number = input('\nSelect a point group:')
        print('Point group selected is', point_group_number, '\n')
        # now get the FZ type and order
        fztype, fzorder = pyEMsoft.so3.getfztypeandorder(point_group_number)
        print('FZ type and order for the selected point group ', point_group_number, 'is', fztype,'and', fzorder,'\n')
        # is it inside the FZ? return a boolean
        insideFZ = pyEMsoft.so3.isinsidefz(rod, fztype, fzorder)
        print('Does Rodrigues point', rod, 'lie in the FZ? \nAnswer: %s' % bool(insideFZ), '\n')

    def test_02_CubochoricNeighbors(self):
        # define an arbitrary quaternion
        q = np.asarray([1, 2, 3, 4], dtype=np.float64)
        # normaliztion of quaternion
        q  = q / pyEMsoft.quaternions.cabs(q)
        # convert to cubochoric coordinates
        cub = pyEMsoft.rotations.qu2cu(q)
        # number of nearest neighbor in each direction (should be an odd number for symmetric meshing)
        nn = 1
        # define the cubneighbor with fortran ordering
        cubneighbor = np.asfortranarray(np.zeros([3, (2*nn+1)**3]), dtype=np.float64)
        # get the cubochoric coordinates of the neighbors
        pyEMsoft.so3.cubochoricneighbors(cubneighbor, nn, cub, 0.1)
        print('Cubochoric coordinates of the neighbors:\n', cubneighbor,'\n')

if __name__ == '__main__':
    unittest.main()