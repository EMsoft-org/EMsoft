# this is an example that can be used to create a crystal structure

import sys
sys.path.append('../')
import pyEMsoft

# define the unitcell usinge typedefs
LatCell = pyEMsoft.typedefs.unitcell()
# set the crystal system and lattice parameters/angles
pyEMsoft.crystal.getlatparm(LatCell)
# set the space group number
pyEMsoft.symmetry.getspacegroup(LatCell)
# set space group setting
pyEMsoft.symmetry.generatesymmetry(LatCell, True)
# set atom types, fractional coordiantesm, site occupation and Debye-Waller factor
pyEMsoft.crystal.getasympos(LatCell)
# input file name (.xtal)
LatCell.fname=input('File name of the crystal file (.xtal):')
# source of crystal data
LatCell.source=input('Source of crystal data:')

# print the unitcell containing crystallographic information
print('\nCrystal System:', LatCell.xtal_system,'\n')
print('Lattice Parameters:\na= %.04f nm\nb= %.04f nm\nc= %.04f nm\n' 
% (LatCell.a, LatCell.b, LatCell.c), '\n')
print('Angles:\n\u03B1= %.04f\u00b0\n\u03B2= %.04f\u00b0\n\u03B3= %.04f\u00b0\n' 
% (LatCell.alpha, LatCell.beta, LatCell.gamma),'\n')
print('Space Group Number:', LatCell.sym_sgnum, '\n')
print('Space Group Setting:', LatCell.sym_sgset, '\n')
print('Number of Atom Types', LatCell.atom_ntype, '\n')
print('Atom Type (atomic number):', LatCell.atom_type[0:LatCell.atom_ntype], '\n')
print('Fractional coordiantes, Site occupation and Debye-Waller factor: \n', 
LatCell.atom_pos[0:LatCell.atom_ntype, 0:5], '\n')
# empty spaces are reserved for more input characters
print('File name:', LatCell.fname.decode('utf-8'), '\n')
print('Source of crystal data:', LatCell.source.decode('utf-8'))
# use routine from HDFsupport to save crystal data in the XtalFolder
pyEMsoft.hdfsupport.savedatahdf(LatCell)




