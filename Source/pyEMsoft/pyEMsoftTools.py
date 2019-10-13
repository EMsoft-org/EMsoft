# some simple tools to have when working with pyEMsoft module
import pyEMsoft 
import numpy as np


class Tools(object):
    """
    Module Tools
    
    
    Some tools to help with pyEMsoft module
    
    """
    @staticmethod
    def get_character_array(ASCII_array):
        """
        CArray = get_character_array(ASCII_array)

        
        Convert numpy array (ASCII) data into character array

        Parameters
        ----------
        ASCII_array : float array
        
        Returns
        -------
        CArray : character array

        """
        if len(ASCII_array.shape) == 3:
            ASCII_Array = ASCII_array.reshape(ASCII_array.shape[0], ASCII_array.shape[1]*ASCII_array.shape[2])
        else:
            ASCII_Array = ASCII_array
        CArray = np.chararray((1, ASCII_Array.shape[1]), itemsize=ASCII_Array.shape[0], unicode=True)
        for j in range(ASCII_Array.shape[1]):
            tempCharacter = ''
            for i in range(ASCII_Array.shape[0]):
                tempCharacter += chr(ASCII_Array[i, j])
            CArray[0, j] = tempCharacter
        if len(ASCII_array.shape) == 3:
            CArray = CArray.reshape(ASCII_array.shape[1], ASCII_array.shape[2])
        return CArray

    @staticmethod
    def get_point_group(SGNUM):
        """
        pgnum = get_point_group(SGNUM)


        Determine the point group a space group number

        Parameters
        ----------
        SGNUM : int
        
        Returns
        -------
        pgnum : int

        """
        SGPG = pyEMsoft.typedefs.sgpg
        pgnum = 0
        for i in range(31, -1, -1):
            if SGPG[i] <= SGNUM:
                pgnum = i+1
                break
        return pgnum

    @staticmethod
    def get_space_string(chr):
        """
        transspace_str=get_space_string(chr)


        Get the corresponding name of the space in strings

        Parameters
        ----------
        chr : str
        
        Returns
        -------
        transspace_str : str

        """
        if chr == 'd':
            transspace_str = 'direct space'
        elif chr == 'r':
            transspace_str = 'reciprocal space'
        elif chr == 'c':
            transspace_str = 'standard cartesian reference frame'
        else:
            print('Undefined space')
        return transspace_str

    @staticmethod
    def get_crystal_system_name(crystal_system_number):
        """
        crystal_system_name = get_crystal_system_name(crystal_system_number)


        Get the corresponding name of the crystal system

        Parameters
        ----------
        crystal_system_number : int
        
        Returns
        -------
        crystal_system_name : str

        """

        if crystal_system_number == 1:
            crystal_system_name = 'Cubic'
        elif crystal_system_number == 2:
            crystal_system_name = 'Tetragonal'
        elif crystal_system_number == 3:
            crystal_system_name = 'Orthorhombic'
        elif crystal_system_number == 4:
            crystal_system_name = 'Hexagonal'
        elif crystal_system_number == 5:
            crystal_system_name = 'Trigonal'
        elif crystal_system_number == 6:
            crystal_system_name = 'Monoclinic'
        elif crystal_system_number == 7:
            crystal_system_name = 'Triclinic'
        elif crystal_system_number == 8:
            crystal_system_name = '2-D Quasi-Crystal'
        elif crystal_system_number == 9:
            crystal_system_name = '3-D Quasi-Crystal'
        else:
            print('Undefined crystal system')
        return crystal_system_name


class ExtractData:
    """
    Module ExtractData

    Extract hdf5 data from crystal structure file or master EBSD file
    
    """

    def __init__(self, master):
        if len(list(master.keys()))==1:
            self.crystaldata = master['CrystalData']

        else:
            self.crystaldata = master['CrystalData']
            self.emdata = master['EMData']
            self.emheader = master['EMheader']
            self.nmlfiles = master['NMLfiles']
            self.nmlparameters = master['NMLparameters']   
            self.crystaldata = master['CrystalData']

    def crystal_data(self):
        """
        Crystal, AtomDict, Info = crystal_data()


        Extract everything about the crystal structure information


        Returns
        -------
        Crystal : dict
        AtomDict : dict
        Info : dict

        """

        Elements = Tools.get_character_array(pyEMsoft.constants.atom_sym)
        AtomData=self.crystaldata['AtomData'][:]
        Atomtypes=self.crystaldata['Atomtypes'][:]
        CreationDate=self.crystaldata['CreationDate'][:]
        Creator=self.crystaldata['Creator'][:]
        CrystalSystem=self.crystaldata['CrystalSystem'][:]
        LatticeParameters=self.crystaldata['LatticeParameters'][:]
        Natomtypes=self.crystaldata['Natomtypes'][:]
        ProgramName=self.crystaldata['ProgramName'][:]
        Source=self.crystaldata['Source'][:]
        SpaceGroupSetting=self.crystaldata['SpaceGroupSetting'][:]
        SpaceGroupNumber=self.crystaldata['SpaceGroupNumber'][:]

        # the Crystal dictionary contains crystallographic information of the structure
        # get point group number
        pgnum = Tools.get_point_group(SpaceGroupNumber.tolist())
        # get space group symbol
        space_group_names = Tools.get_character_array(pyEMsoft.typedefs.sym_sgname)
        # a dictionary for lattice parameters and angles
        LatticeParameters={'a':LatticeParameters.tolist()[0],'b':LatticeParameters.tolist()[1],'c':LatticeParameters.tolist()[2],
        'alpha':LatticeParameters.tolist()[3],'beta':LatticeParameters.tolist()[4],'gamma':LatticeParameters.tolist()[5]}
        # nexted dictioanry for crystal information
        Crystal={'Crystal System': Tools.get_crystal_system_name(CrystalSystem[0]), 'Lattice Parameters': LatticeParameters,
        'Point Group': pgnum,'Space Group Number': SpaceGroupNumber.tolist(),'Space Group Name':space_group_names[0,SpaceGroupNumber[0]-1],
        'Space Group Setting': SpaceGroupSetting.tolist()}
        # Atom information
        atomkeys=['Element', 'x', 'y', 'z', 'Site occupation parameter', 'Debye-Waller factor']
        AtomDict={}
        for i in range(Natomtypes[0]):
            Element_symbol=Elements[0,Atomtypes[i]-1]
            Atom = AtomData[0:5,i].tolist()
            Atom.insert(0, Element_symbol)
            AtomDict.update({str('Atom %d'%(i+1)): dict(zip(atomkeys,Atom))})

        # other info
        Info={'Creator': Creator[0].decode('utf-8'), 'Creation Date': CreationDate[0].decode('utf-8'), 
        'Program Name': ProgramName[0].decode('utf-8'), 'Source':Source[0].decode('utf-8')}
        return Crystal, AtomDict, Info

    def ebsd_master(self):
        """
        master_info, master_pattern = ebsd_master()


        Extract 2D master patterns (stereographic projection and Lambert projection) as well as related information


        Returns
        -------
        master_info : dict
        master_pattern : float array

        """
        ebsd_master = self.emdata['EBSDmaster']
        
        BetheParameters = ebsd_master['BetheParameters'][:]
        EkeVs = ebsd_master['EkeVs'][:]
        Z2percent = ebsd_master['Z2percent'][:]
        lastEnergy = ebsd_master['lastEnergy'][:]
        mLPNH = ebsd_master['mLPNH'][:]
        mLPSH = ebsd_master['mLPSH'][:]
        masterSPNH = ebsd_master['masterSPNH'][:]
        masterSPSH = ebsd_master['masterSPSH'][:]
        numEbins = ebsd_master['numEbins'][:]
        master_info={'Bethe Parameters': BetheParameters.tolist(), 'Energy Bins (kVs)': EkeVs.tolist(), 
        'Z2percent': Z2percent.tolist(), 'lastEnergy': lastEnergy.tolist()}
        accum_e =np.sum(self.emdata['MCOpenCL/accum_e'], axis=(0,1)).astype('float32')
        
        LPNH=mLPNH[0]
        LPSH=mLPSH[0]
        for i in range(1,mLPNH.shape[0]):
            LPNH+=mLPNH[i]
            LPSH+=mLPNH[i]
            
        accum_e = accum_e / np.sum(accum_e)
        LPNH = np.average(LPNH, axis=0, weights=accum_e)
        LPSH = np.average(LPSH, axis=0, weights=accum_e)
        SPNH = np.average(masterSPNH, axis=0, weights=accum_e)
        SPSH = np.average(masterSPSH, axis=0, weights=accum_e)

        master_pattern={'SPNH': SPNH,'SPSH': SPSH,'LPNH': LPNH,'LPSH': LPSH}
        return master_info,master_pattern
