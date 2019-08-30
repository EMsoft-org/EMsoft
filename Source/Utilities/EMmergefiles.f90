! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are 
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list 
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this 
!        list of conditions and the following disclaimer in the documentation and/or 
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
!        of its contributors may be used to endorse or promote products derived from 
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

!--------------------------------------------------------------------------
! EMsoft:EMmergefiles.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMmergefiles 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief perform a merge of two HDF5 files using system calls to the h5copy program
!
!> @details This program is a utility program to merge data sets from a Monte Carlo
!> and a master pattern file (ECP or EBSD), to convert older data files to the new
!> single HDF file format.  The program also copies the CrystalData group from the 
!> regular .xtal file.
! 
!> @date 08/18/16 MDG 1.0 original
!> @date 10/01/16 MDG 1.1 added CrystalData group
!--------------------------------------------------------------------------
program EMmergefiles

use local
use io
use HDFsupport
use ISO_C_BINDING
use error
use stringconstants

integer(kind=irg)                       :: numarg       !< number of command line arguments
integer(kind=irg)                       :: iargc        !< external function for command line
character(fnlen)                        :: arg          !< to be read from the command line
integer(kind=irg)                       :: mode, hdferr, nlines
character(512)                          :: cmd, cmd2          !< output command
character(fnlen)                        :: h5copypath, xtalfile, energyfile, groupname, dataset
character(fnlen)                        :: infile1, infile2, outfile          !< file names
character(fnlen)                        :: progname, progdesc, mess, xtalname
logical                                 :: f_exists, readonly
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)
type(HDFobjectStackType),pointer        :: HDF_head


progname = 'EMmergefiles.f90'
progdesc = 'Utility: Merge two EMsoft HDF files into a single file'

! print some information
call EMsoft(progname, progdesc)

numarg = iargc()
mode = 0

if (numarg.eq.1) then ! there is at least one argument
  call getarg(1,arg)
  mess = 'Found the following argument: '//trim(arg); call Message(mess,"(/A/)")
! does the argument start with a '-' character?    
  if (arg(1:1).eq.'-') then
    if (trim(arg).eq.'-h') then
      call Message(' This is a utility program that can be used to convert Monte Carlo and master pattern files', frm = "(A)")
      call Message(' to a format in which both data sets are present in the same HDF file; this is the default', frm = "(A)")
      call Message(' file organization starting in release 3.1 and beyond. ', frm = "(A)")
      call Message(' ', frm = "(A)")
      call Message(' The program can be called in the following ways: ', frm = "(/A)")
      call Message('        '//trim(progname)//' -h', frm = "(A)")
      call Message('        '//trim(progname)//' -ECP infile1 infile2 outfile', frm = "(A)")
      call Message('        '//trim(progname)//' -EBSD infile1 infile2 outfile', frm = "(A)")
      call Message(' ')
      call Message(' ', frm = "(A)")
      call Message(' -h: print this message and quit', frm = "(A)")
      call Message(' ', frm = "(A)")
      call Message(' When there are four arguments, [e.g., -MC infile1 infile2 outfile] then the complete', frm = "(A)")
      call Message(' data from both input files will be copied to the outfile, but with the proper HDF group', frm = "(A)")
      call Message(' organization. The first input file MUST be a Monte Carlo file, the second either an EBSD .', frm = "(A)")
      call Message(' or ECP master file', frm = "(A)")
      call Message(' ', frm = "(A)")
    else
      call Message(' Unknown command line option',frm = "(A//)")
      STOP
    end if
  end if
else
  if (numarg.ne.4) then 
    call Message(' This program requires 1 or 4 command line arguments',frm = "(A//)")
    STOP
  else 
    call getarg(1,arg)
    mess = 'Found the following argument: '//trim(arg); call Message(mess,"(/A/)")
    if (arg(1:1).ne.'-') then
      call Message(' first argument must be -ECP or -EBSD',frm = "(A//)")
      STOP
    else
      if (arg(3:3).eq.'B') then 
        mode = 1
      else if (arg(3:3).eq.'C') then 
        mode = 2
      else 
        call Message(' Unknown command line argument',frm = "(A)")
        STOP
      end if
      call getarg(2,arg)
      infile1 = trim(arg)
      call getarg(3,arg)
      infile2 = trim(arg)
      call getarg(4,arg)
      outfile = trim(arg)

! make sure the input files exist in the current folder
      inquire(file=infile1, exist=f_exists)
      if (.not.f_exists) then
        call FatalError('EMmergefiles','Monte Carlo input file does not exist')
      end if

      inquire(file=infile2, exist=f_exists)
      if (.not.f_exists) then
        call FatalError('EMmergefiles','Master pattern input file does not exist')
      end if

! extract the xtalname variable from the MC file so that we can insert the 
! CrystalData group in the output file as well...
      energyfile = trim(infile1)
      energyfile = EMsoft_toNativePath(energyfile)
      nullify(HDF_head%next)
      call h5open_EMsoft(hdferr)
! open the MC file using the default properties.
      readonly = .TRUE.
      hdferr =  HDF_openFile(energyfile, HDF_head, readonly)
! open the namelist group
groupname = SC_NMLparameters
      hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCCLNameList
      hdferr = HDF_openGroup(groupname, HDF_head)

! read the xtalname data set from the Monte Carlo file and close the file
dataset = SC_xtalname
      call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
      xtalname = trim(stringarray(1))
      call HDF_pop(HDF_head,.TRUE.)
      call h5close_EMsoft(hdferr)

! make sure that the .xtal file exists. 
      xtalfile = trim(EMsoft_getXtalpathname())//trim(xtalname)
      xtalfile = EMsoft_toNativePath(xtalfile)
      inquire(file=xtalfile, exist=f_exists)
      if (.not.f_exists) then
        call FatalError('EMmergefiles','Crystal structure input file not found in XtalFolder')
      end if


! start copying data sets from the Monte Carlo file
      h5copypath = trim(EMsoft_geth5copypath())//' -p -v '
      cmd = trim(h5copypath)//' -i "'//trim(infile1)
      cmd = trim(cmd)//'" -o "'//trim(outfile)

      cmd2 = trim(cmd)//'" -s "/EMData" -d "/EMData/MCOpenCL"'
      call system(trim(cmd2))

      cmd2 = trim(cmd)//'" -s "/EMheader" -d "/EMheader/MCOpenCL"'
      call system(trim(cmd2))

      cmd2 = trim(cmd)//'" -s "/NMLfiles" -d "/NMLfiles/MCOpenCLNML"'
      call system(trim(cmd2))

      cmd2 = trim(cmd)//'" -s "/NMLparameters/MCCLNameList" -d "/NMLparameters/MCCLNameList"'
      call system(trim(cmd2))

! add the CrystalData group from the .xtal crystal structure file
      h5copypath = trim(EMsoft_geth5copypath())//' -p -v '
      cmd = trim(h5copypath)//' -i "'//trim(xtalfile)
      cmd = trim(cmd)//'" -o "'//trim(outfile)

      cmd2 = trim(cmd)//'" -s "/CrystalData" -d "/CrystalData"'
      call system(trim(cmd2))


! and copy data sets from the master file
      cmd = trim(h5copypath)//' -i "'//trim(infile2)
      cmd = trim(cmd)//'" -o "'//trim(outfile)

      if (mode.eq.1) then  ! we're merging a Monte Carlo file and an EBSD master pattern file
        cmd2 = trim(cmd)//'" -s "/EMData" -d "/EMData/EBSDmaster"'
        call system(trim(cmd2))

        cmd2 = trim(cmd)//'" -s "/EMheader" -d "/EMheader/EBSDmaster"'
        call system(trim(cmd2))

        cmd2 = trim(cmd)//'" -s "/NMLfiles" -d "/NMLfiles/EBSDmasterNML"'
        call system(trim(cmd2))

        cmd2 = trim(cmd)//'" -s "/NMLparameters/EBSDMasterNameList" -d "/NMLparameters/EBSDMasterNameList"'
        call system(trim(cmd2))
      end if
      if (mode.eq.2) then  ! we're merging a Monte Carlo file and an ECP master pattern file
        cmd2 = trim(cmd)//'" -s "/EMData" -d "/EMData/ECPmaster"'
        call system(trim(cmd2))

        cmd2 = trim(cmd)//'" -s "/EMheader" -d "/EMheader/ECPmaster"'
        call system(trim(cmd2))

        cmd2 = trim(cmd)//'" -s "/NMLfiles" -d "/NMLfiles/ECPmasterNML"'
        call system(trim(cmd2))

        cmd2 = trim(cmd)//'" -s "/NMLparameters/ECPMasterNameList" -d "/NMLparameters/ECPMasterNameList"'
        call system(trim(cmd2))
      end if
      call Message(' ', frm = "(A)")
    end if
  end if
end if

end program EMmergefiles
