
import sys
sys.path.append('../')
import pyEMsoft
from pyEMsoftTools import Tools
import numpy as np
import math

# Examples here are taken from A text book written by Professor Marc De Graef:
# De Graef, M., 2003. Introduction to conventional transmission electron microscopy.
# Cambridge University Press.

# Example 1.1
# define the unitcell usinge typedefs
LatCell = pyEMsoft.typedefs.unitcell()
# set the crystal system and lattice parameters/angles 
# use a=0.5, c=1
pyEMsoft.crystal.getlatparm(LatCell)
# calculate the reciprocal/direct metric/structure tensors 
pyEMsoft.crystal.calcmatrices(LatCell)
print('\nDirect metric tensor of the tetragonal crystal with a=0.5, c=1 is:\n', 
np.round(LatCell.dmt,2), '\n')

# Example 1.2
# distance between the two points is equal to the length of the vector 
# connecting the two points in direct space
p1 = np.array([0.5, 0, 0.5])
p2 = np.array([0.5, 0.5, 0])
v = p1-p2
# define the space (d=direct space, r=reciprocal space, c=cartesian reference frame)
space = 'd'
v_length = pyEMsoft.crystal.calclength(LatCell, v, space)
print('Length between', p1, 'and', p2, 'is', v_length, '(nm) in the', 
Tools.get_space_string(space))

# Example 1.3 
# dot product and angle between two vectors in the direct space
v1 = np.array([1, 2, 0])
v2 = np.array([3, 1, 1])
v1_dot_v2 = pyEMsoft.crystal.calcdot(LatCell, v1, v2, space)
print('Dot product of', v1, 'and', v2, 'is', v1_dot_v2, '(nmˆ2) in the', 
Tools.get_space_string(space))
# angle between two vectors in the direct space
a = pyEMsoft.crystal.calcangle(LatCell, v1, v2, space)
print('The angle between', v1, 'and', v2, 'is %4f' % math.degrees(a))

# Example 1.5 & 1.9
print('\nReciprocal metric tensor of the tetragonal crystal with a=0.5,c=1 is:\n', 
np.round(LatCell.rmt,2), '\n')
# The reciprocal lattice vectors are just rows of the the recirpocal metric tensor

# Example 1.6
# given two plane normals 
n1 = np.asarray([1, 2, 0])
n2 = np.asarray([3, 1, 1])
space = 'r'
a = pyEMsoft.crystal.calcangle(LatCell, n1, n2, space)
print('The angle between', n1, 'and', n2, 'is %4f' % math.degrees(a), '\n')

# Example 1.8
v = np.asarray([1, 1, 4])
n = np.asarray([0, 0, 0], dtype=np.float32, order='F')
inspace = 'd'
outspace = 'r'
pyEMsoft.crystal.transspace(LatCell, v, n, inspace, outspace)
print('The', v, 'in the ', Tools.get_space_string(inspace), 'has been converted to', 
n,'in', Tools.get_space_string(outspace), '\n')

# Example 1.10
v1 = np.asarray([1, 1, 0], dtype=np.float32)
v2 = np.asarray([1, 1, 1], dtype=np.float32)
output_vector = np.asarray([0, 0, 0], dtype=np.float32, order='F')
# define the space of input vector (direct space)
inspace = 'd'
# define the space of the output vector (reciprocal space)
outspace = 'r'
pyEMsoft.crystal.calccross(LatCell, v1, v2, output_vector, inspace, outspace, True)
print('The output vector cross product is', output_vector, 
'in the', Tools.get_space_string(outspace))
pyEMsoft.crystal.calccross(LatCell, v1, v2, output_vector, inspace, inspace, True)
print('or',output_vector,'in the', Tools.get_space_string(inspace), '\n')

# Example 1.12
g1 = np.asarray([0, 0, 0], dtype=np.float32, order='F')
g2 = np.asarray([0, 0, 0], dtype=np.float32, order='F')
output_vector = np.asarray([0, 0, 0], dtype=np.float32, order='F')
# determine the reciprocal space vectors for v1 and v2
pyEMsoft.crystal.transspace(LatCell, v1, g1, inspace, outspace)
pyEMsoft.crystal.transspace(LatCell, v2, g2, inspace, outspace)
# redefine the space of input vector (reciprocal space)
inspace = 'r'
# redefine the space of the output vector (direct space)
outspace = 'd'
pyEMsoft.crystal.calccross(LatCell, g1, g2, output_vector, inspace, outspace, True)
print('The output vector cross product is', output_vector, 
'in the', Tools.get_space_string(outspace))
pyEMsoft.crystal.calccross(LatCell, g1, g2, output_vector, inspace, inspace, True)
print('or',output_vector,'in the', Tools.get_space_string(inspace), '\n')
