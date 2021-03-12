# some simple tools to have when working with pyEMsoft module
from EMsoft import pyEMsoft
import numpy as np
import h5py as h5
import os
import matplotlib.pyplot as plt


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
            ASCII_Array = ASCII_array.reshape(
                ASCII_array.shape[0], ASCII_array.shape[1]*ASCII_array.shape[2])
        else:
            ASCII_Array = ASCII_array
        CArray = np.chararray(
            (1, ASCII_Array.shape[1]), itemsize=ASCII_Array.shape[0], unicode=True)
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
        if len(list(master.keys())) == 1:
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
        AtomData = self.crystaldata['AtomData'][:]
        Atomtypes = self.crystaldata['Atomtypes'][:]
        CreationDate = self.crystaldata['CreationDate'][:]
        Creator = self.crystaldata['Creator'][:]
        CrystalSystem = self.crystaldata['CrystalSystem'][:]
        LatticeParameters = self.crystaldata['LatticeParameters'][:]
        Natomtypes = self.crystaldata['Natomtypes'][:]
        ProgramName = self.crystaldata['ProgramName'][:]
        Source = self.crystaldata['Source'][:]
        SpaceGroupSetting = self.crystaldata['SpaceGroupSetting'][:]
        SpaceGroupNumber = self.crystaldata['SpaceGroupNumber'][:]

        # the Crystal dictionary contains crystallographic information of the structure
        # get point group number
        pgnum = Tools.get_point_group(SpaceGroupNumber.tolist())
        # get space group symbol
        space_group_names = Tools.get_character_array(
            pyEMsoft.typedefs.sym_sgname)
        # a dictionary for lattice parameters and angles
        LatticeParameters = {'a': LatticeParameters.tolist()[0], 'b': LatticeParameters.tolist()[1], 'c': LatticeParameters.tolist()[2],
                             'alpha': LatticeParameters.tolist()[3], 'beta': LatticeParameters.tolist()[4], 'gamma': LatticeParameters.tolist()[5]}
        # nexted dictioanry for crystal information
        Crystal = {'Crystal System': Tools.get_crystal_system_name(CrystalSystem[0]), 'Lattice Parameters': LatticeParameters,
                   'Point Group': pgnum, 'Space Group Number': SpaceGroupNumber.tolist(), 'Space Group Name': space_group_names[0, SpaceGroupNumber[0]-1],
                   'Space Group Setting': SpaceGroupSetting.tolist()}
        # Atom information
        atomkeys = ['Element', 'x', 'y', 'z',
                    'Site occupation parameter', 'Debye-Waller factor']
        AtomDict = {}
        for i in range(Natomtypes[0]):
            Element_symbol = Elements[0, Atomtypes[i]-1]
            Atom = AtomData[0:5, i].tolist()
            Atom.insert(0, Element_symbol)
            AtomDict.update(
                {str('Atom %d' % (i+1)): dict(zip(atomkeys, Atom))})

        # other info
        Info = {'Creator': Creator[0].decode('utf-8'), 'Creation Date': CreationDate[0].decode('utf-8'),
                'Program Name': ProgramName[0].decode('utf-8'), 'Source': Source[0].decode('utf-8')}
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
        master_info = {'Bethe Parameters': BetheParameters.tolist(), 'Energy Bins (kVs)': EkeVs.tolist(),
                       'Z2percent': Z2percent.tolist(), 'lastEnergy': lastEnergy.tolist()}
        accum_e = np.sum(
            self.emdata['MCOpenCL/accum_e'], axis=(0, 1)).astype('float32')

        LPNH = mLPNH[0]
        LPSH = mLPSH[0]
        for i in range(1, mLPNH.shape[0]):
            LPNH += mLPNH[i]
            LPSH += mLPNH[i]

        accum_e = accum_e / np.sum(accum_e)
        LPNH = np.average(LPNH, axis=0, weights=accum_e)
        LPSH = np.average(LPSH, axis=0, weights=accum_e)
        SPNH = np.average(masterSPNH, axis=0, weights=accum_e)
        SPSH = np.average(masterSPSH, axis=0, weights=accum_e)

        master_pattern = {'SPNH': SPNH,
                          'SPSH': SPSH, 'LPNH': LPNH, 'LPSH': LPSH}
        return master_info, master_pattern


def loadOptimizationData(emdatapath, inputtype, EBSD, Opt):
    """ 
    EBSD_Opt, quaternion =loadOptimizationData(emdatapath, inputtype, EBSD)

    Load global optimization output data with default output file name

    Parameters
    ----------
    emdatapath      : str
    inputtype       : str
    EBSD            : class
    Opt             : class

    Returns
    -------
    EBSD_Opt        : class
    quaternion      : float array
    Ftensor         : float array
    """
    # read the output data file
    Data = h5.File(os.path.join(emdatapath, EBSD.datafile))
    eulerangles = np.reshape(Data['/EMData/EBSD/eulerangles'][:], (3))  
    PC = np.reshape((Data['/EMData/EBSD/PC'][:]), (3))
    Ftensor=np.array([1,0,0,0,1,0,0,0,1],dtype=float)
    # read the optimized pc and Euler angles
    if EBSD.applydeformation=='n':
        Ftensor = np.reshape(Data['/EMData/EBSD/Ftensor'][:], (9))
        if Opt.hybrid=='y':
            eulerangles = np.reshape(Data['/EMData/EBSD/eulerangles_NMS'][:], (3))  
            PC = np.reshape((Data['/EMData/EBSD/PC_NMS'][:]), (3))
            Ftensor = np.reshape(Data['/EMData/EBSD/Ftensor_NMS'][:], (9))

    # optimization code output all orientation in TSL format
    # convert the Bruker and Oxford back by subtracting 90 degrees from phi1
    if inputtype == 'BrukerHDF':
        string = 'bruker'
        eulerangles[0] = eulerangles[0]-np.pi/2
        quaternion = pyEMsoft.rotations.eu2qu(eulerangles)
    elif inputtype == 'TSLHDF':
        string = 'tsl'
        quaternion = pyEMsoft.rotations.eu2qu(eulerangles)
    elif inputtype == 'OxfordHDF':
        string = 'hkl'
        eulerangles[0] = eulerangles[0]-np.pi/2
        quaternion = pyEMsoft.rotations.eu2qu(eulerangles)
    else:
        print('Undefined input type')
    # update the pattern center
    EBSD_Opt = PCtoEMsoftPC(string, PC, EBSD)
    return EBSD_Opt, quaternion, Ftensor


def EMEBSDGlobalOptimizationUpdate(string, Opt, EBSD, Pattern):
    """ 
    loadOptimizationData(string, Opt, EBSD, Pattern)

    Update the template file for global optimization 

    Parameters
    ----------
    string          : str
    Opt             : class
    EBSD            : class
    Pattern         : class

    """
    # copy the template file
    os.system('EMEBSDGlobalOpt -t')
    # open input file
    fin = open("EMEBSDGlobalOpt.template", "rt")
    # output file to write the result to
    fout = open("EMEBSDGlobalOpt.nml", "wt")
    # HDFstrings that contains the patterns
    if Pattern.inputtype == 'EMEBSD':
        HDFstring = "HDFstrings = 'EMData' 'EBSD' 'EBSDPatterns' '' '' '' '' '' '' ''"
    elif Pattern.inputtype == 'BrukerHDF':
        HDFstring = "HDFstrings = '"+string + \
            "' 'EBSD' 'Data' 'RawPatterns' '' '' '' '' '' ''"
    elif Pattern.inputtype == 'TSLHDF':
        HDFstring = "HDFstrings = '"+string+"' 'EBSD' 'Data' 'Pattern' '' '' '' '' '' ''"
    else:
        print('Undefined inputtype')

    # the default values in the template file
    old = ("applyDeformation = 'n'","NP=60", "itermax=100", "refresh=0", "bound = 0.001,2,2",
           "hybrid='n'","masterfile = 'Ni-master-20kV.h5'", "datafile='data.h5'",
           "targetfile = 'Ni-scan-LargeArea.h5'",
           "numsx = 480", "numsy = 480", "ipf_wd = 101", "ipf_ht = 151",
           "inputtype = 'EMEBSD'",
           "HDFstrings = 'EMData' 'EBSD' 'EBSDPatterns' '' '' '' '' '' '' ''",
           "patx = 0", "paty = 0", "nthreads = 1", "energymax = 20",
           "energymin = 10", "delta=50", "scalingmode = 'not'", "gammavalue = 0", "binning = 1",
           "makedictionary = 'n'", "maskpattern = 'n'", "nregions = 10")

    # replace the default values in the template file
    new = ("applyDeformation = '"+EBSD.applyDeformation+"'", 'NP =' + str(Opt.NP), 'itermax =' + str(Opt.itermax),
           'refresh= ' + str(Opt.refresh), 'bound = '+str(Opt.bound)[1:-1], "hybrid='"+Opt.hybrid+"'",
           "masterfile ='"+EBSD.masterfile+"'", "datafile= '"+EBSD.datafile+"'",
           "targetfile = '"+Pattern.targetfile+"'",
           'numsx = ' + str(EBSD.numsx), 'numsy = ' + str(EBSD.numsy),
           'ipf_wd = ' + str(Pattern.ipf_wd), 'ipf_ht = ' +
           str(Pattern.ipf_ht),
           'inputtype = '+"'"+Pattern.inputtype+"'", HDFstring,
           'patx = '+str(Pattern.patx), 'paty = ' +
           str(Pattern.paty), 'nthreads = '+str(EBSD.nthreads),
           'energymax = ' +
           str(EBSD.energymax), 'energymin = ' + str(EBSD.energymin),
           'delta='+str(EBSD.delta), 'scalingmode ='+"'" +
           EBSD.scalingmode+"'", 'gammavalue =' +
           str(EBSD.gammavalue), "binning = "+str(EBSD.binning),
           "makedictionary ='"+EBSD.makedictionary+"'",
           "maskpattern ='"+EBSD.maskpattern+"'",
           'nregions = '+str(EBSD.nregions))
    # replace the values
    for line in fin:
        for check, rep in zip(old, new):
            line = line.replace(check, rep)
        fout.write(line)
    # close input and output files
    fin.close()
    fout.close()


def loadPattern(inputtype, data, path):
    """ 
    enl, patterndata, angles = loadPattern(inputtype, data, path)

    Load metadata from pattern file

    Parameters
    ----------
    inputtype       : str
    data            : class
    path            : str

    Returns
    -------
    enl             : class
    patterndata     : class
    angles          : float array 

    """
    if inputtype == 'TSLHDF':
        enl, patterndata, angles = loadTSLHDF(data, path)
    elif inputtype == 'BrukerHDF':
        enl, patterndata, angles = loadBrukerHDF(data, path)
    else:
        print("Undefined Data Type")
    pattern.targetfile=path
    return enl, patterndata, angles


def loadSEM(inputtype, data, path):
    """ 
    PatternQuality, SEM = loadSEM(inputtype, data, path)

    Load pattern quality map and SEM array from pattern file

    Parameters
    ----------
    inputtype       : str
    data            : class
    path            : str

    Returns
    -------
    PatternQuality  : float array 
    SEM             : nt array
    """
    if inputtype == 'TSLHDF':
        PatternQuality, SEM = loadTSLHDFSEM(data, path)
    elif inputtype == 'BrukerHDF':
        PatternQuality, SEM = loadBrukerHDFSEM(data, path)
    else:
        print("Undefined Data Type")
    return PatternQuality, SEM


def loadBrukerHDF(str, path):
    """ 
    enl, patterndata, angles = loadBrukerHDF(str, path)

    Load metadata from BrukerHDF pattern file

    Parameters
    ----------
    str             : str
    path            : str

    Returns
    -------
    enl             : class
    patterndata     : class
    angles          : float array 

    """
    Data = h5.File(path, 'r')
    enl = pyEMsoft.Namelisttypedefs.EBSDNameListType
    patterndata = pyEMsoft.Namelisttypedefs.EBSDDIpreviewNameListType
    # Euler angle convention
    enl.eulerconvention = 'hkl'
    # Detector pixel size
    enl.delta = 50  # um

    enl.thetac = Data['/'+str+'/EBSD/Header/CameraTilt'][()]
    enl.numsy = Data['/'+str+'/EBSD/Header/PatternHeight'][()]
    enl.numsx = Data['/'+str+'/EBSD/Header/PatternWidth'][()]

    PC_X = np.mean(Data['/'+str+'/EBSD/Data/PCX'][:])
    PC_Y = np.mean(Data['/'+str+'/EBSD/Data/PCY'][:])
    DD = np.mean(Data['/'+str+'/EBSD/Data/DD'][:])

    enl = PCtoEMsoftPC('bruker', [PC_X, PC_Y, DD], enl)

    patterndata.ipf_wd = Data['/'+str+'/EBSD/Header/NCOLS'][()]
    patterndata.ipf_ht = Data['/'+str+'/EBSD/Header/NROWS'][()]
    patterndata.inputtype = 'BrukerHDF'

    phi1 = (Data['/'+str+'/EBSD/Data/phi1'])
    phi2 = (Data['/' +
                 str+'/EBSD/Data/phi2'])
    Phi = (Data['/' +
                str+'/EBSD/Data/PHI'])

    # total number of patterns
    numangles = patterndata.ipf_wd*patterndata.ipf_ht

    # convert the Euler angles to quaternions
    angles = np.zeros((4, numangles))
    for i in range(numangles):
        angles[:, i] = pyEMsoft.rotations.eu2qu(
            np.deg2rad([phi1[i], Phi[i], phi2[i]]))
    return enl, patterndata, angles


def loadBrukerHDFSEM(str, path):
    """ 
    PatternQuality, SEM=  loadBrukerHDFSEM(str, path)

    Load pattern quality map and SEM array from BrukerHDF file

    Parameters
    ----------
    str             : str
    path            : str

    Returns
    -------
    PatternQuality  : float array 
    SEM             : nt array
    """
    Data = h5.File(path, 'r')
    wd = Data['/'+str+'/EBSD/Header/NCOLS'][()]
    ht = Data['/'+str+'/EBSD/Header/NROWS'][()]
    PatternQuality = np.zeros((ht, wd))
    SEM = np.zeros((2, wd*ht), dtype=int)
    PatternQuality = np.reshape(
        Data['/'+str+'/EBSD/Data/RadonQuality'][:], (ht, wd))
    SEM[0, :] = Data['/'+str+'/SEM/SEM IX'][:]
    SEM[1, :] = Data['/'+str+'/SEM/SEM IY'][:]
    return PatternQuality, SEM


def loadTSLHDF(str, path):
    """ 
    enl, patterndata, angles = loadBrukerHDF(str, path)

    Load metadata from TSLHDF pattern file

    Parameters
    ----------
    str             : str
    path            : str

    Returns
    -------
    enl             : class
    patterndata     : class
    angles          : float array 

    """
    Data = h5.File(path, 'r')
    enl = pyEMsoft.Namelisttypedefs.EBSDNameListType
    patterndata = pyEMsoft.Namelisttypedefs.EBSDDIpreviewNameListType
    # Euler angle convention
    enl.eulerconvention = 'tsl'
    # Detector pixel size
    enl.delta = 59.2  # um

    enl.thetac = Data['/' +
                      str+'/EBSD/Header/Camera Elevation Angle'][0]
    enl.numsy = Data['/' +
                     str+'/EBSD/Header/Pattern Height'][0]
    enl.numsx = Data['/' +
                     str+'/EBSD/Header/Pattern Width'][0]
    PC_X = Data['/' +
                str+'/EBSD/Header/Pattern Center Calibration/x-star'][0]

    PC_Y = Data['/' +
                str+'/EBSD/Header/Pattern Center Calibration/y-star'][0]

    DD = Data['/' +
              str+'/EBSD/Header/Pattern Center Calibration/z-star'][0]

    enl = PCtoEMsoftPC('tsl', [PC_X, PC_Y, DD], enl)

    patterndata.ipf_wd = Data['/' +
                              str+'/EBSD/Header/nColumns'][0]
    patterndata.ipf_ht = Data['/' +
                              str+'/EBSD/Header/nRows'][0]
    patterndata.inputtype = 'TSLHDF'
    phi1 = (Data['/' +
                 str+'/EBSD/Data/Phi1'])
    phi2 = (Data['/' +
                 str+'/EBSD/Data/Phi2'])
    Phi = (Data['/' +
                str+'/EBSD/Data/Phi'])

    # total number of patterns
    numangles = patterndata.ipf_wd*patterndata.ipf_ht

    # convert the Euler angles to quaternions
    angles = np.zeros((4, numangles))
    for i in range(numangles):
        angles[:, i] = pyEMsoft.rotations.eu2qu(
            np.asarray([phi1[i], Phi[i], phi2[i]]))
    return enl, patterndata, angles


def loadTSLHDFSEM(str, path):
    """ 
    PatternQuality, SEM=  loadTSLHDFSEM(str, path)

    Load pattern quality map and SEM array from TSLHDF file

    Parameters
    ----------
    str             : str
    path            : str

    Returns
    -------
    PatternQuality  : float array 
    SEM             : nt array
    """
    Data = h5.File(path, 'r')
    wd = Data['/'+str+'/EBSD/Header/nColumns'][0]
    ht = Data['/'+str+'/EBSD/Header/nRows'][0]
    PatternQuality = np.zeros((ht, wd))
    SEM = np.zeros((2, wd*ht), dtype=int)
    PatternQuality = np.reshape(
        Data['/'+str+'/EBSD/Data/IQ'][:], (ht, wd))

    SEM[0, :] = np.divide(Data['/'+str+'/EBSD/Data/X Position']
                          [:], Data['/'+str+'/EBSD/Header/Step X'][:])
    SEM[1, :] = np.divide(Data['/'+str+'/EBSD/Data/Y Position']
                          [:], Data['/'+str+'/EBSD/Header/Step Y'][:])

    return PatternQuality, SEM


def getSingleEBSDPattern(str, EBSD, Pattern, path):
    """ 
    TargetPattern =  getSingleEBSDPattern(str, EBSD, Pattern, path)

    Get a single EBSD pattern from a pattern file

    Parameters
    ----------
    str             : str
    EBSD            : class
    Pattern         : class
    path            : str

    Returns
    -------
    TargetPattern   : array 
    """
    ID = Pattern.paty*Pattern.ipf_wd+Pattern.patx
    if str == 'EMData':
        Data = h5.File(os.path.join(path, 'EBSDout.h5'), 'r')

        if EBSD.makedictionary == 'n':
            TargetPattern = (Data['/EMData/EBSD/EBSDPatterns'][0, :, :])
        else:
            TargetPattern = np.reshape(
                Data['/EMData/EBSD/EBSDPatterns'][0, :], (EBSD.numsy, EBSD.numsx))
    else:
        Data = h5.File(path, 'r')
        if Pattern.inputtype == 'TSLHDF':
            TargetPattern = Data['/' + str+'/EBSD/Data/Pattern'][ID, :, :]
        elif Pattern.inputtype == 'BrukerHDF':
            TargetPattern = Data['/' + str +
                                 '/EBSD/Data/RawPatterns'][ID, :, :]
    return TargetPattern


def EMEBSDnamelistUpdate(EBSD):
    """ 
    EMEBSDnamelistUpdate(EBSD)

    Update EMEBSD nml file with EBSD namelist class

    Parameters
    ----------
    EBSD            : class
    """
    os.system('EMEBSD -t')
    # open input file
    fin = open("EMEBSD.template", "rt")
    # output file to write the result to
    fout = open("EMEBSD.nml", "wt")
    # replace the default values in the template file
    old = ("L = 15000.0", "thetac = 10.0", "delta = 50.0", "numsx = 0",
           "numsy = 0", "xpc = 0.0", "ypc = 0.0", "energymin = 5.0",
           "energymax = 20.0", "eulerconvention = 'tsl'",
           "master.h5", "scalingmode = 'not'", "gammavalue = 1.0",
           "includebackground = 'y'", "makedictionary = 'n'", "maskpattern = 'n'", "nregions = 10")
    new = ('L =' + str(EBSD.L), 'thetac =' + str(EBSD.thetac),
           'delta = ' + str(EBSD.delta), 'numsx = ' + str(
        EBSD.numsx), 'numsy = ' + str(EBSD.numsy), 'xpc = ' + str(EBSD.xpc),
        'ypc = ' + str(EBSD.ypc), 'energymin = ' + str(EBSD.energymin), 'energymax = ' + str(
        EBSD.energymax), 'eulerconvention ='+"'"+EBSD.eulerconvention+"'",
        EBSD.masterfile, 'scalingmode ='+"'"+EBSD.scalingmode +
        "'", 'gammavalue =' +
        str(EBSD.gammavalue),
        "includebackground ='"+EBSD.includebackground+"'",
        "makedictionary ='"+EBSD.makedictionary +
        "'", "maskpattern ='"+EBSD.maskpattern+"'",
        'nregions = '+str(EBSD.nregions))
    for line in fin:
        for check, rep in zip(old, new):
            line = line.replace(check, rep)
        fout.write(line)
    # close input and output files
    fin.close()
    fout.close()


def createAngleFile(emdatapath, angle_type, Pattern, AnglesMatrix):
    """ 
    createAngleFile(emdatapath, angle_type, Pattern, AnglesMatrix)

    Create Euler angle file for EMEBSD program

    Parameters
    ----------
    emdatapath          : str
    angle_type          : str
    Pattern             : class
    AnglesMatrix        : float array

    """
    if AnglesMatrix.size > 4:
        quaternion = AnglesMatrix[:, Pattern.paty*Pattern.ipf_wd+Pattern.patx]
    else:
        quaternion = AnglesMatrix

    if angle_type == 'eu':
        Angles = np.rad2deg(pyEMsoft.rotations.qu2eu(quaternion))
        head = "eu"
    elif angle_type == 'qu':
        Angles = quaternion
        head = "qu"
    else:
        print("Undefined angle representation")

    f = open(os.path.join(emdatapath, 'testeuler' + "." + 'txt'), "w+")

    if Angles.ndim == 1:
        n_angles = 1
        text_to_write = [head+'\n', str(n_angles)+'\n']
        text_to_write.append(str(Angles)[1:-1]+'\n')
    else:
        n_angles = Angles.shape[1]
        text_to_write = [head+'\n', str(n_angles)+'\n']
        for angle in range(n_angles):
            text_to_write.append(str(Angles[:, angle])[1:-1]+'\n')

    f.writelines(text_to_write)


def PCtoEMsoftPC(str, PC, enl):
    """ 
    enl =  PCtoEMsoftPC(str, PC, enl)

    Convert pattern center to EMsoft convention

    Parameters
    ----------
    str             : str
    PC              : class
    enl             : class

    Returns
    -------
    enl             : class
    """
    if str == 'tsl':
        enl.xpc = -enl.numsx*(PC[0]-0.5)
        enl.ypc = enl.numsx*PC[1]-enl.numsy*0.5
        enl.L = enl.numsx*enl.delta*PC[2]
    elif str == 'hkl':
        enl.xpc = -enl.numsx*(PC[0]-0.5)
        enl.ypc = enl.numsy*(PC[1]-0.5)
        enl.L = enl.numsx*enl.delta*PC[2]
    elif str == 'bruker':
        enl.xpc = -enl.numsx*(PC[0]-0.5)
        enl.ypc = enl.numsy*(0.5-PC[1])
        enl.L = enl.numsy*enl.delta*PC[2]
    else:
        print('undefined pattern center convention')
    return enl


def EMsoftPCtoPC(str, enl):
    """ 
    enl =  EMsoftPCtoPC(str, PC, enl)

    Convert EMsoft pattern center to vendor's convention

    Parameters
    ----------
    str             : str
    PC              : class
    enl             : class

    Returns
    -------
    enl             : class
    """
    PC = np.zeros(3)
    if str == 'tsl':
        PC[0] = -enl.xpc/enl.numsx+0.5
        PC[1] = (enl.ypc+enl.numsy*0.5)/enl.numsx
        PC[2] = enl.L / (enl.numsx*enl.delta)
    elif str == 'hkl':
        PC[0] = -enl.xpc/enl.numsx+0.5
        PC[1] = enl.ypc/enl.numsy+0.5
        PC[2] = enl.L / (enl.numsx*enl.delta)
    elif str == 'bruker':
        PC[0] = -enl.xpc/enl.numsx+0.5
        PC[1] = 0.5-enl.ypc/enl.numsy
        PC[2] = enl.L / (enl.numsy*enl.delta)
    else:
        print('undefined pattern center convention')
    return PC


def getGrayscale(binned):
    """ 
    bpatint =  getGrayscale(binned)

    Get grayscale pattern [0,255]

    Parameters
    ----------
    binned          : array

    Returns
    -------
    bpatint         : int array
    """
    mi = binned.min()
    ma = binned.max()
    bpatint = np.asarray(((binned - mi) / (ma-mi))*255.0, dtype=int)
    return bpatint


def circular_mask(ht, wd):
    """ 
    mask =  circular_mask(ht, wd)

    Create a circular mask for a pattern

    Parameters
    ----------
    binned          : array

    Returns
    -------
    mask            : int array
    """
    center = [int(wd/2), int(ht/2)]
    radius = min(center[0], center[1], wd-center[0], ht-center[1])
    Y, X = np.ogrid[:ht, :wd]
    dist_from_center = np.sqrt((X - center[0])**2 + (Y-center[1])**2)
    mask = dist_from_center <= radius
    return mask
