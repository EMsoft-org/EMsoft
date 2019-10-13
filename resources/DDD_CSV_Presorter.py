#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun  5 07:29:09 2017

@author: joseph
@brief: Take in a CSV that contains DDD simulation results and output a 4D presorted array
in h5 format.
"""

import pandas as pd
import h5py
import numpy as np

def round25(x):
    # return round(x*4)/4
    return round(x*16)/16

rotate  = True            # does the data need to be rotated
propdir = 'Z'            # direction of beam propagation
posscale= 1              # scale point grid, if not in nm
disscale= 1              # scale of displacement values, if not in nm

frac    = 1 # Lower the resolution of the grid, which fraction of points are included

# If the data needs to be rotated to a proper reference frame, this is the matrix describing the
# relationship between the crystal and sample axes
rotmat = np.matrix([[1, 0, 0],[0, 1, 0],[0, 0, 1]])
rotmat = rotmat.transpose()

# path to input file 
path = 'full/input/path/here' #full path to the input file
inpath = path + ".txt"        # extension for input path
print(inpath)


atomdata  = pd.read_csv(inpath, sep=' ', skipinitialspace=True,header=None)#, skiprows=9,)
maxIndex = atomdata.shape[0]
print('read file')
print(maxIndex)


# rotate data if necessary
ad = atomdata.as_matrix()
if(rotate):
    for i in range(0,maxIndex):
        ad[i,3:6] = np.matmul(ad[i,3:6], rotmat) * disscale
        ad[i,0:3] = np.matmul(ad[i,0:3], rotmat) * posscale

        if i%1000000 == 0: print(i)
print('rotated matrix')

# now the 2D array needs to be sorted: 
maxs = np.amax(ad,0)
mins = np.amin(ad,0)
xxdim = (maxs[0] - mins[0])/frac 
yydim = (maxs[1] - mins[1])/frac 
zzdim = (maxs[2] - mins[2])/frac 

displacements = np.zeros((5,int(xxdim)+ 1,int(yydim)+ 1,int(zzdim)+ 1),dtype=float)


for i in range(0,maxIndex):
    xx = round25(ad[i,0]/frac+xxdim/2)
    yy = round25(ad[i,1]/frac+yydim/2)
    zz = round25(ad[i,2]/frac+zzdim/2)

    if (xx%1 == 0.0 and yy%1 == 0.0 and zz%1 == 0.0):
        displacements[0:3,int(xx),int(yy),int(zz)] = ad[i,3:6]
        displacements[3,int(xx),int(yy),int(zz)] = 1
        displacements[4,int(xx),int(yy),int(zz)] = 1

    if i%1000000 == 0: print(i)

# sort displacements for different prop dirs
if  (propdir == 'Z'):
    displacements = np.transpose(displacements,[3,2,1,0])
elif(propdir == 'Y'):
    # (disp,z,x,y)
    for i in range(0,sidel):
        for j in range(0,sidel):
            for k in range(0,2*depth+1):
                displacements[:,i,j,k] = (displacements[2,i,j,k],displacements[0,i,j,k],\
                    displacements[1,i,j,k],displacements[3,i,j,k],displacements[4,i,j,k])
    displacements = np.transpose(displacements,[2,1,3,0])


elif(propdir == 'X'):
    # (disp,z,y,x)    
    for i in range(0,sidel):
        for j in range(0,sidel):
            for k in range(0,2*depth+1):
                displacements[:,i,j,k] = (displacements[2,i,j,k],displacements[1,i,j,k],\
                    displacements[0,i,j,k],displacements[3,i,j,k],displacements[4,i,j,k])
    displacements = np.transpose(displacements,[1,2,3,0])

print('sorted to 4D')

outpath = path + ".h5"
f = h5py.File(outpath,'w')
dset = f.create_dataset('Data',data=displacements)
