;
; Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are 
; permitted provided that the following conditions are met:
;
;     - Redistributions of source code must retain the above copyright notice, this list 
;        of conditions and the following disclaimer.
;     - Redistributions in binary form must reproduce the above copyright notice, this 
;        list of conditions and the following disclaimer in the documentation and/or 
;        other materials provided with the distribution.
;     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
;        of its contributors may be used to endorse or promote products derived from 
;        this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; ###################################################################
;--------------------------------------------------------------------------
; EMsoft:EBSDreadHDFdatafile.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDreadHDFdatafile.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Reads the HDF data files produced by the EMMC.f90 and EMEBSDmaster.f90 programs
;
;> @date 03/19/14 MDG 1.0 first attempt 
;> @date 04/02/15 MDG 2.0 modfied from EBSDreaddatafile to cover HDF formatted files
;> @date 05/07/15 MDG 2.1 modified to accommodate changes in path names for the entire EMsoft package
;> @date 10/29/15 MDG 2.2 added support for ECP master files
;> @date 10/31/15 MDG 2.3 removed widget fields and redirected all output to status widget
;> @date 11/09/15 MDG 2.4 disabled separate MCFile display option; incorporated Kossel pattern option
;> @date 10/11/16 MDG 3.0 changed input to new joint HDF5 format for Monte Carlo and master patterns
;> @date 12/21/18 MDG 3.1 added Tag parsing to check for existence of HDF5 data set
;--------------------------------------------------------------------------
pro EBSDreadHDFdatafile,dummy
;
;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata

; the next common block contains all the raw data needed to generate the EBSD patterns
common EBSD_rawdata, accum_e, accum_z, mLPNH, mLPSH

Core_Print,' ',/blank
SEMdata.MCMPboth = 1

EMdatapathname = ''

  Core_Print,'Reading data file '+SEMdata.mpfilename

  EMdatapathname = Core_getenv(/data)

; first make sure that this is indeed an HDF file
  res = H5F_IS_HDF5(SEMdata.pathname+'/'+SEMdata.mpfilename)
  if (res eq 0) then begin
    Core_Print,'  This is not an HDF file ! ',/blank
    goto,skipall
  endif

; it is an HDF5 file, so let's first get the complete Tags structure
  hdftags = H5_PARSE(SEMdata.pathname+'/'+SEMdata.mpfilename)

; ok, so it is an HDF file; let's open it
  file_id = H5F_OPEN(SEMdata.pathname+'/'+SEMdata.mpfilename)
  if (file_id eq -1L) then begin
    Core_Print,'  Error opening file',/blank
    goto, skipall
  endif 

; first we need to determine what kind of master file this is (EBSD, ECP, TKD, or Kossel)
; and then we open the corresponding group in the EMheader group
  SEMdata.mpfiletype=-1
  res = H5G_GET_NMEMBERS(file_id,'EMheader')
  for i=0,res[0]-1 do begin
    gname = H5G_GET_MEMBER_NAME(file_id,'EMheader',i)
    if (gname eq 'EBSDmaster') then begin
      SEMdata.mpfiletype=1
      group_id = H5G_open(file_id,'EMheader/EBSDmaster')
    endif
    
    if (gname eq 'ECPmaster') then begin
      SEMdata.mpfiletype=2
      group_id = H5G_open(file_id,'EMheader/ECPmaster')
    endif
    
    if (gname eq 'Kosselmaster') then begin
      SEMdata.mpfiletype=3
      group_id = H5G_open(file_id,'EMheader/Kosselmaster')
    endif

    if (gname eq 'TKDmaster') then begin
      SEMdata.mpfiletype=4
      group_id = H5G_open(file_id,'EMheader/TKDmaster')
    endif
    
  endfor
  if (SEMdata.mpfiletype eq -1) then begin
    Core_Print,'  This file does not contain an EBSD, ECP, TKD, or Kossel master pattern',/blank
    goto, skipall
  endif

;  try to open and read the ProgramName dataset
  if (Core_Tag_Exists(hdftags.EMheader.EBSDMASTER,'ProgramName') eq 1) then begin
    dset_id = H5D_open(group_id,'ProgramName')
    z = H5D_read(dset_id) 
    progname = strtrim(z[0],2)
    H5D_close,dset_id
      Core_Print,' ->File generated by program '+progname+'<-'
  endif

  WIDGET_CONTROL, set_value=SEMdata.mpfiletypestring[SEMdata.mpfiletype], SEMwidget_s.mpfiletype

; open and read the Version dataset
  if (Core_Tag_Exists(hdftags.EMheader.EBSDMASTER,'Version') eq 1) then begin
    dset_id = H5D_open(group_id,'Version')
    z = H5D_read(dset_id) 
    scversion = strtrim(z[0],2)
    H5D_close,dset_id
    SEMdata.scversion = strtrim(scversion,2)
      Core_Print,'      Version identifier : '+scversion 
  endif

; close the EMheader group
  H5G_close,group_id

; display the file size in Mb 
  Core_print,'      MP data file size : '+string(float(SEMdata.mpfilesize)/1024./1024.,FORMAT="(F8.2)")+' Mb'

; =====================================================
; =====================================================
; open and read the Monte Carlo dataset in the EMheader groups
  if (SEMdata.mpfiletype ne 3) then begin ; Kossel mode does not have any associated Monte Carlo data sets 
    if (SEMdata.mpfiletype lt 3) then group_id = H5G_open(file_id,'EMheader/MCOpenCL') else group_id = H5G_open(file_id,'EMheader/MCfoil')
    Core_Print,'Reading Monte Carlo data sets '
    SEMdata.Esel = 0

  ;  open and read the ProgramName dataset
   if (Core_Tag_Exists(hdftags.EMheader.MCOPENCL,'ProgramName') eq 1) then begin
     dset_id = H5D_open(group_id,'ProgramName')
      z = H5D_read(dset_id) 
      mcprogname = strtrim(z[0],2)
      H5D_close,dset_id
        Core_Print,' ->Monte Carlo data generated by program '+mcprogname+'<-'
   endif

    H5G_close,group_id

  ; structure file name
    if (SEMdata.mpfiletype lt 3) then group_id = H5G_open(file_id,'NMLparameters/MCCLNameList') else group_id = H5G_open(file_id,'NMLparameters/MCCLfoilNameList')

  ; open and read the xtalname dataset
    if (Core_Tag_Exists(hdftags.NMLparameters.MCCLNameList,'xtalname') eq 1) then begin
     dset_id = H5D_open(group_id,'xtalname')
      z = H5D_read(dset_id) 
      SEMdata.xtalname = strtrim(z[0],2)
      H5D_close,dset_id
      Core_print,'      Structure file name : '+SEMdata.xtalname
    endif

  ; open and read the mode dataset, to distinguish between EBSD and ECP data
    if (SEMdata.mpfiletype lt 3) then begin
      dset_id = H5D_open(group_id,'mode')
      m = H5D_read(dset_id)
      if (m[0] eq 'full') then SEMdata.EBSDorECP = 0 else SEMdata.EBSDorECP = 1
      H5D_close,dset_id
    end 

  ; open and read the numsx dataset 
    dset_id = H5D_open(group_id,'numsx')
    SEMdata.mcimx = (long(H5D_read(dset_id))-1L)/2L
    SEMdata.mcimy = SEMdata.mcimx
    H5D_close,dset_id

  ; open and read the totnum_el dataset
    dset_id = H5D_open(group_id,'totnum_el')
    SEMdata.mctotale = long(H5D_read(dset_id))
    H5D_close,dset_id

  ; open and read the EkeV dataset
    dset_id = H5D_open(group_id,'EkeV')
    SEMdata.mcenergymax = double(H5D_read(dset_id))
    H5D_close,dset_id
    SEMdata.voltage = SEMdata.mcenergymax

    if (SEMdata.EBSDorECP eq 0) then begin
  ; open and read the Ehistmin dataset
      dset_id = H5D_open(group_id,'Ehistmin')
      SEMdata.mcenergymin = double(H5D_read(dset_id))
      H5D_close,dset_id

  ; open and read the Ebinsize dataset
      dset_id = H5D_open(group_id,'Ebinsize')
      SEMdata.mcenergybinsize = double(H5D_read(dset_id))
      H5D_close,dset_id
    end else begin
      SEMdata.mcenergymin = SEMdata.mcenergymax
      SEMdata.mcenergybinsize = 0.D0
    endelse

  ; open and read the depthmax dataset
    dset_id = H5D_open(group_id,'depthmax')
    SEMdata.mcdepthmax = double(H5D_read(dset_id))
    H5D_close,dset_id

  ; open and read the depthstep dataset
    dset_id = H5D_open(group_id,'depthstep')
    SEMdata.mcdepthstep = double(H5D_read(dset_id))
    H5D_close,dset_id

    if (SEMdata.EBSDorECP eq 0) then begin
      dset_id = H5D_open(group_id,'sig')
      SEMdata.mcvangle = double(H5D_read(dset_id))
      H5D_close,dset_id
    end else begin
      dset_id = H5D_open(group_id,'sigstart')
      SEMdata.mcsigstart = double(H5D_read(dset_id))
      H5D_close,dset_id

      dset_id = H5D_open(group_id,'sigend')
      SEMdata.mcsigend = double(H5D_read(dset_id))
      H5D_close,dset_id

      dset_id = H5D_open(group_id,'sigstep')
      SEMdata.mcsigstep = double(H5D_read(dset_id))
      H5D_close,dset_id
    endelse

  ; open and read the omega dataset
    dset_id = H5D_open(group_id,'omega')
    SEMdata.mchangle = double(H5D_read(dset_id))
    H5D_close,dset_id

  ; open and read the MCmode dataset
    if (Core_Tag_Exists(hdftags.NMLparameters.MCCLNameList,'MCmode') eq 1) then begin
      dset_id = H5D_open(group_id,'MCmode')
      res = strtrim(H5D_read(dset_id))
      H5D_close,dset_id
      if (res eq 'CSDA') then SEMdata.mcmode = 'CSDA' else SEMdata.mcmode = 'DLOS'
    endif

    Core_print,'      Lambert dimensions : '+string(2*SEMdata.mcimx+1,format="(I5)")+' by '+string(2*SEMdata.mcimy+1,format="(I5)")
    Core_print,'      Incident beam voltage [kV] '+string(SEMdata.voltage,format="(F7.2)")
    Core_print,'      Monte Carlo mode : '+SEMdata.mcmode

  ; close the group
    H5G_close,group_id

  ; and open the EMData group
    if (SEMdata.mpfiletype lt 3) then group_id = H5G_open(file_id,'EMData/MCOpenCL') else group_id = H5G_open(file_id,'EMData/MCfoil')

  ; open and read the numEbins dataset
    if (SEMdata.EBSDorECP eq 0) then begin
      dset_id = H5D_open(group_id,'numEbins')
      SEMdata.mcenergynumbin= long(H5D_read(dset_id))
      H5D_close,dset_id
      Core_print,'      Sample tilt angles omega/sigma [degrees] : '+string(SEMdata.mchangle,format="(F7.2)")+'/'+string(SEMdata.mcvangle,format="(F7.2)")
      Core_print,'      Min/Max energy [keV] : '+string(SEMdata.mcenergymin,format="(F7.2)")+'/'+string(SEMdata.mcenergymax,format="(F7.2)")
      Core_print,'      Energy binsize [keV] : '+string(SEMdata.mcenergybinsize,format="(F7.2)")
      Core_print,'      Number of energy bins : '+string(SEMdata.mcenergynumbin,format="(I5)")
    end else begin
      dset_id = H5D_open(group_id,'numangle')
      SEMdata.mcenergynumbin= long(H5D_read(dset_id))
      H5D_close,dset_id
      Core_print,'      Electron energy [keV] : '+string(SEMdata.mcenergymax,format="(F7.2)")
      Core_print,'      Number of beam tilt angles : '+string(SEMdata.mcenergynumbin,format="(I5)")
      Core_print,'      Beam tilt range start/end/step [degrees] : '+string(SEMdata.mcsigstart,format="(F7.2)")+'/'+string(SEMdata.mcsigend,format="(F7.2)")+'/'+string(SEMdata.mcsigstep,format="(F7.2)")
    endelse

  ; open and read the numzbins dataset
    dset_id = H5D_open(group_id,'numzbins')
    SEMdata.mcdepthnumbins = long(H5D_read(dset_id))
    H5D_close,dset_id

    Core_print,'      Maximum integration depth [nm] : '+string(SEMdata.mcdepthmax,format="(F7.2)")
    Core_print,'      Integration depth step size [nm] : '+string(SEMdata.mcdepthstep,format="(F7.2)")
    Core_print,'      Number of depth bins : '+string(SEMdata.mcdepthnumbins,format="(I5)")
    
  ; and finally, we read the actual data arrays accum_e
    dset_id = H5D_open(group_id,'accum_e')
    accum_e = long(H5D_read(dset_id))
    H5D_close,dset_id

  ; close the group
    H5G_close,group_id

  ; total number of BSE electrons
    SEMdata.mcbse = total(accum_e)
    Core_print,'      Total number of incident electrons : '+string(SEMdata.mctotale,format="(I12)")
    Core_print,'      Number of BSEs : '+string(SEMdata.mcbse,format="(I12)")

    sz = size(accum_e,/dimensions)
      Core_Print,'      Size of accum_e data array : '+string(sz[0],format="(I5)")+' x'+string(sz[1],format="(I5)")+' x'+string(sz[2],format="(I5)")

  ; and initialize the coordinate arrays for the Lambert transformation
    Core_LambertS2C,reform(accum_e[0,*,*]),/mc
    Core_LambertS2SP,reform(accum_e[0,*,*]),/mc

  end; else H5G_close,group_id  ; end of the Monte Carlo data sets
; =====================================================
; =====================================================

; ok, we're done reading the Monte Carlo data, if any, so we continue with the remainder of the master pattern data

; open the NMLparameters/somethingorother group
  namelistnames = ['','NMLparameters/EBSDMasterNameList','NMLparameters/ECPMasterNameList','NMLparameters/KosselMasterNameList','NMLparameters/TKDMasterNameList']
  group_id = H5G_open(file_id,namelistnames[SEMdata.mpfiletype])

; if this is a Kossel master pattern file we must make sure that the Kosselmode is normal, not thicks
  if (SEMdata.mpfiletype eq 3) then begin
    dset_id = H5D_open(group_id,'Kosselmode')
    z = H5D_read(dset_id) 
    H5D_close,dset_id
    if (strtrim(z[0],2) ne 'normal') then begin
      Core_print,'This Kossel master pattern file has program mode thicks instead of normal '
      Core_print,'and can not be used in this display program'
; and close the file
      H5G_close,group_id
      H5F_close,file_id
      goto,skipall 
    end
  end

; commented out on 1/18/17 by MDG; not sure if this is still needed...
; outname (to reset the current filename to the correct EMdatapathname)
; dset_id = H5D_open(group_id,'outname')
; z = H5D_read(dset_id)
; res = strtrim(z[0],2)
; H5D_close,dset_id
; SEMdata.mpfilename = strmid(res,0,strlen(res))

; npx, npy
  dset_id = H5D_open(group_id,'npx')
  z = H5D_read(dset_id) 
  res = long(z[0])
  H5D_close,dset_id
  SEMdata.mpimx = res
  SEMdata.mpimy = res
  SEMdata.Asymsel = -1

; close the group
  H5g_close,group_id

; open the EMData group
  if (SEMdata.mpfiletype eq 1) then group_id = H5G_open(file_id,'EMData/EBSDmaster')
  if (SEMdata.mpfiletype eq 2) then group_id = H5G_open(file_id,'EMData/ECPmaster')
  if (SEMdata.mpfiletype eq 3) then group_id = H5G_open(file_id,'EMData/Kosselmaster')
  if (SEMdata.mpfiletype eq 4) then group_id = H5G_open(file_id,'EMData/TKDmaster')

; various variables
  if (SEMdata.mpfiletype eq 3) then begin
    dset_id = H5D_open(group_id,'numthick')
    z = H5D_read(dset_id) 
    res = long(z[0])
    H5D_close,dset_id
    SEMdata.mcenergynumbin = res
    Core_print,'      Number of thickness values : '+string(SEMdata.mcenergynumbin,format="(I5)")

    dset_id = H5D_open(group_id,'startthick')
    z = H5D_read(dset_id) 
    res = long(z[0])
    H5D_close,dset_id
    SEMdata.mcdepthmax = res
    Core_print,'      Start thickness value : '+string(SEMdata.mcdepthmax,format="(F5.2)")

    dset_id = H5D_open(group_id,'thickinc')
    z = H5D_read(dset_id) 
    res = long(z[0])
    H5D_close,dset_id
    SEMdata.mcdepthstep = res
    Core_print,'      Thickness step size : '+string(SEMdata.mcdepthmax,format="(F5.2)")
  end

; and finally the results arrays 
  dset_id = H5D_open(group_id,'mLPNH')
  mLPNH = H5D_read(dset_id) 
  H5D_close,dset_id
  dset_id = H5D_open(group_id,'mLPSH')
  mLPSH = H5D_read(dset_id) 
  H5D_close,dset_id

; close the group
  H5G_close,group_id


; next we need to read a few fields from the CrystalData group
  if (SEMdata.mpfiletype ne 3) then begin
    group_id = H5G_open(file_id,'CrystalData')

; number of atom types in the unit cell
    dset_id = H5D_open(group_id,'Natomtypes')
    z = H5D_read(dset_id) 
    res = long(z[0])
    H5D_close,dset_id
    SEMdata.numset= res

; atomic numbers for asymmetric unit
    dset_id = H5D_open(group_id,'Atomtypes')
    atnum = H5D_read(dset_id) 
    H5D_close,dset_id
    SEMdata.atnum(0:SEMdata.numset-1) = atnum(0:SEMdata.numset-1)
    H5G_close,group_id
  end

; and close the file
  H5F_close,file_id

; resize the mLPNH/mLPSH arrays to the correct number of dimensions
  if ((SEMdata.mpfiletype eq 1) or (SEMdata.mpfiletype eq 4)) then begin
    sz = size(mLPNH)
    if (sz[0] eq 3) then begin
      mLPNH = reform(mLPNH,sz[1],sz[2],sz[3],1)
      mLPSH = reform(mLPSH,sz[1],sz[2],sz[3],1)
    endif
    sz = size(mLPNH,/dimensions)
    Core_Print,'      Size of mLPNH data array : '+string(sz[0],format="(I5)")+' x'+string(sz[1],format="(I5)") +' x'+string(sz[2],format="(I5)") +' x'+string(sz[3],format="(I5)") 
    Core_LambertS2C,reform(mLPNH[*,*,0,0]),/mp
    Core_LambertS2SP,reform(mLPNH[*,*,0,0]),/mp
  end 
  if (SEMdata.mpfiletype eq 2) then begin
    sz = size(mLPNH)
    if (sz[0] eq 2) then begin
      mLPNH = reform(mLPNH,sz[1],sz[2],1)
      mLPSH = reform(mLPSH,sz[1],sz[2],1)
    endif
    sz = size(mLPNH,/dimensions)
    Core_Print,'      Size of mLPNH data array : '+string(sz[0],format="(I5)")+' x'+string(sz[1],format="(I5)") +' x'+string(sz[2],format="(I5)") 
    Core_LambertS2C,reform(mLPNH[*,*,0]),/mp
    Core_LambertS2SP,reform(mLPNH[*,*,0]),/mp
    SEMdata.detnumsy = SEMdata.detnumsx
  end
  if (SEMdata.mpfiletype eq 3) then begin
    sz = size(mLPNH,/dimensions)
    Core_Print,'      Size of mLPNH data array : '+string(sz[0],format="(I5)")+' x'+string(sz[1],format="(I5)") +' x'+string(sz[2],format="(I5)") 
    Core_LambertS2C,reform(mLPNH[*,*,0]),/mp
    Core_LambertS2SP,reform(mLPNH[*,*,0]),/mp
  end


; activate the necessary buttons
  WIDGET_CONTROL, SEMwidget_s.MPbutton, sensitive=1
  WIDGET_CONTROL, SEMwidget_s.detector, sensitive=1

; and tell the user we're done.
  Core_print,' '
  Core_Print,'Completed reading data file',/blank


skipall:

end
