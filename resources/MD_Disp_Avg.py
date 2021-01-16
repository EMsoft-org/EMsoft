#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: Joseph Tessmer
@brief: Take in a CSV that contains MD simulation results and output a 4D presorted array
in h5 format.

Note that the code expects data in the following format:
  0     1    2  3  4     5     6   7   8
index, type, x, y, z, energy, dx, dy, dz
in this case the displacement is the sum of the t=0 displacement and the time-dependent displacement.

The file reader also skips the standard 9-row LAMMPS header, if this isn't present change the header 
var to 0.
"""

import pandas as pd
import h5py
import numpy as np
import math

def getrotmat(x,y,z):
    res = np.zeros((3,3))
    # normalize each vector
    xden = np.sqrt(x[0]*x[0] + x[1]*x[1] + x[2]*x[2])
    yden = np.sqrt(y[0]*y[0] + y[1]*y[1] + y[2]*y[2])
    zden = np.sqrt(z[0]*z[0] + z[1]*z[1] + z[2]*z[2])
    res[:,0] = x[:]/xden
    res[:,1] = y[:]/yden
    res[:,2] = z[:]/zden
    return res



# ============ User-defined variables =============

step    = 4                # size of the unit cell in same units as values
propdir = 'Z'              # direction of beam propagation (you can also rotate the crystal to acheive this)

outpath = "output_file_name.h5"                            # output file name
inpath = "/path/to/csv/with/atom/data.txt"                 # path to input file
header = 9                                                 # number of header rows in input file

# If the axes your atom positions are defined along do not align with the axes
# of the simulation volume, you should put the relationship between the crystal
# and the sample axes here and set rotate to True.
rotate = False            # does the data need to be rotated
x = (1,0,0)               # Crystal X
y = (0,1,0)               # Crystal Y 
z = (0,0,1)               # Crystal Z

scale = False    # do the displacement values need to be rescaled?
scaler = (1,1,1) # do not use this unless you have a reason.
# =========== End user-defined variables ===========


# Read in MD positions
#                           0     1   2  3  4     5     6   7   8
# should be formatted as index, type, x, y, z, energy, dx, dy, dz

# read in the input data
# ====================================================================================
atomdata  = pd.read_csv(inpath, sep=' ', skipinitialspace=True,header=None, skiprows=header)
numatoms = atomdata.shape[0]
print(atomdata.shape)
ad = atomdata.values
maxIndex = numatoms
# ====================================================================================

if (rotate):
    rotmat = getrotmat(x,y,z)
    rotmat = rotmat.astype(float)
    # adnew = np.zeros((maxIndex,5))
    for i in range(0,maxIndex):
        ad[i,6:9] = np.matmul(ad[i,6:9], rotmat)
        ad[i,2:5] = np.matmul(ad[i,2:5], rotmat)
        # add in the t=0 displacement here

# now we need to get the extent of the data:
mins = np.amin(ad,axis = 0)
maxs = np.amax(ad,axis = 0)

# generate the displacement grid
# this looks like: dx, dy, dz, futureuse, futureuse
displacements = np.zeros((5,int(np.ceil((maxs[2]-mins[2])/step)),\
    int(np.ceil((maxs[3]-mins[3])/step)),\
    int(np.ceil((maxs[4]-mins[4])/step))),dtype=float)


# loop over the atoms to assign them to a displacement point:
for i in range(0,maxIndex):
    # compute which x,y,z point this atom belongs to:
    # (we assign them to the value to the LEFT)
    # normalize the values:
    ad[i,2] = ad[i,2] - mins[2]
    ad[i,3] = ad[i,3] - mins[3]
    ad[i,4] = ad[i,4] - mins[4]

    # get the cell this atom belongs to
    xx = int(np.floor(ad[i,2]/step))
    yy = int(np.floor(ad[i,3]/step))
    zz = int(np.floor(ad[i,4]/step))

    # get the number of atoms already averaged at this point
    nprev = displacements[4,xx,yy,zz]

    # average the displacement at this point
    displacements[0:3,xx,yy,zz] = (ad[i,6:9] + (displacements[0:3,xx,yy,zz]*nprev))/(nprev + 1)
    displacements[4,xx,yy,zz] += 1
    displacements[3,xx,yy,zz] = 1

    if (i%1000000 == 0): print(i)


# Print some info about the # of atoms in each cell
minat = np.array(displacements[4,:,:,:])
minat = list(minat.flatten())
print(minat.count(0),minat.count(1),minat.count(2),np.ceil((maxs[2]-mins[2])/step)\
    *np.ceil((maxs[3]-mins[3])/step)*np.ceil((maxs[4]-mins[4])/step))
print(minat.count(0)/(np.ceil((maxs[2]-mins[2])/step)*np.ceil((maxs[3]-mins[3])/step)\
    *np.ceil((maxs[4]-mins[4])/step)))

print('Computed displacements at each point')
# save the displacements as a 4D array in an HDF5 file
if (scale):
    # rescale the data
    displacements[0:3,:,:,:] = (displacements[0:3,:,:,:]) * scaler

# Fortran is column-major while python is row-major
if  (propdir == 'Z'):
    # (disp,x,y,z)
    displacements = np.transpose(displacements,[3,2,1,0])
elif(propdir == 'Y'):
    # (disp,z,x,y)
    for i in range(0,int(np.ceil((maxs[2]-mins[2])/step))):
        for j in range(0,int(np.ceil((maxs[3]-mins[3])/step))):
            for k in range(0,int(np.ceil((maxs[4]-mins[4])/step))):
                # displacements[:,i,j,k] = (displacements[2,i,j,k],displacements[0,i,j,k],\
                #     displacements[1,i,j,k],displacements[3,i,j,k],displacements[4,i,j,k])
                displacements[:,i,j,k] = (displacements[0,i,j,k],displacements[2,i,j,k],\
                    displacements[1,i,j,k],displacements[3,i,j,k],displacements[4,i,j,k])
    displacements = np.transpose(displacements,[2,1,3,0])


elif(propdir == 'X'):
    # (disp,z,y,x)
    for i in range(0,int(np.ceil((maxs[2]-mins[2])/step))):
        for j in range(0,int(np.ceil((maxs[3]-mins[3])/step))):
            for k in range(0,int(np.ceil((maxs[4]-mins[4])/step))):
                displacements[:,i,j,k] = (displacements[2,i,j,k],displacements[1,i,j,k],\
                    displacements[0,i,j,k],displacements[3,i,j,k],displacements[4,i,j,k])
    displacements = np.transpose(displacements,[1,2,3,0])

f = h5py.File(outpath,'w')
dset = f.create_dataset('Data'       ,data=displacements)