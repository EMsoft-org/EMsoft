;
; Copyright (c) 2013-2024, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:write_mrc.pro
;--------------------------------------------------------------------------
;
; PROGRAM: write_mrc.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief; create an .mrc file with a CBED pattern in it
;
;> @date 11/29/20 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro MRCinit,MRCHeader,FEIHeaders, numPatterns
;
; initialize the mrc FEIHeaders and MRCHeader structures; these must 
; be filled by the calling program. 
;
; written by MDG, 11/29/20 
;

;/**
; * @brief this was taken from the FEI1 extended header definition in 
;    https://ftp.vsg3d.com/private/MASTERS/Velox/MRC2014%20File%20Image%20Format%20Specification%20-%20306687.pdf
; */
FEIHeaders = replicate({FEIstruct,  $
    ; Image, System and Application
    MetadataSize : long(768), $
    MetadataVersion : long(0), $
    BitMask1 : long(0), $ 
    TimeStamp : double(0), $
    MicroscopeType : '                ', $
    Dnumber : '                ', $
    Application : '                ', $
    ApplicationVersion : '                ', $
    ; Gun
    HT : double(0), $   ; BitMask1 += 2L^5
    Dose : double(0), $
    ; Stage 
    AlphaTilt : double(0), $
    BetaTilt : double(0), $
    XStage : double(0), $ ; BitMask1 += 2L^9
    YStage : double(0), $ ; BitMask1 += 2L^10
    ZStage  : double(0), $ ; BitMask1 += 2L^11
    TiltAxisAngle : double(0), $
    DualAxisRotation : double(0), $
    ; Pixel Size 
    PixelSizeX : double(0), $
    PixelSizeY : double(0), $
    Spacer1 : '                                                ', $
    ; Optics
    Defocus : double(0), $ ; BitMask1 += 2L^22
    STEMDefocus: double(0), $
    AppliedDefocus: double(0), $
    InstrumentMode : long(1), $
    ProjectionMode : long(1), $
    ObjectiveLensMode : 'HM              ', $
    HighMagnificationMode : 'SA              ', $
    ProbeMode : long(1), $
    EFTEMon : byte(0), $
    magnification : double(0), $
    BitMask2 : long(0), $
    CameraLength : double(0), $ ; BitMask2 += 2L^0
    SpotIndex : long(0), $
    IlluminatedArea : double(0), $
    Intensity : double(0), $
    ConvergenceAngle : double(0), $   ; in degrees !!! BitMask2 += 2L^4
    IlluminationMode : 'Probe           ', $
    WideConvergenceAngleRange : byte(0), $
    ; EFTEM Imaging
    SlitInserted : byte(0), $
    SlitWidth : double(0), $
    AccelerationVoltageOffset : double(0), $
    DriftTubeVoltage : double(0), $
    EnergyShift : double(0), $
    ; Image Shifts 
    ShiftOffsetX : double(0), $
    ShiftOffsetY : double(0), $
    ShiftX : double(0), $
    ShiftY : double(0), $
    ; Camera 
    IntegrationTime : double(0), $
    BinningWidth : long(0), $
    BinningHeight : long(0), $
    CameraName : '                ', $
    ReadoutAreaLeft : long(0), $
    ReadoutAreaTop : long(0), $
    ReadoutAreaRight : long(0), $
    ReadoutAreaBottom : long(0), $
    CetaNoiseReduction : byte(0), $
    CetaFramesSummed : long(0), $
    DirectDetectorElectronCounting : byte(0), $
    DirectDetectorAlignFrames : byte(0), $
    CameraParamReserved0 : long(0), $
    CameraParamReserved1 : long(0), $
    CameraParamReserved2 : long(0), $
    CameraParamReserved3 : long(0), $
    BitMask3 : long(0), $
    CameraParamReserved4 : long(0), $
    CameraParamReserved5 : long(0), $
    CameraParamReserved6 : long(0), $
    CameraParamReserved7 : long(0), $
    CameraParamReserved8 : long(0), $
    CameraParamReserved9 : long(0), $
    PhasePlate : byte(0), $
    ; STEM 
    STEMDetectorName : '                ', $
    Gain : double(0), $
    Offset : double(0), $
    STEMParamReserved0 : long(0), $ 
    STEMParamReserved1 : long(0), $ 
    STEMParamReserved2 : long(0), $ 
    STEMParamReserved3 : long(0), $ 
    STEMParamReserved4 : long(0), $ 
    ; Scan Settings
    DwellTime : double(0), $
    FrameTime : double(0), $
    ScanSizeLeft : long(0), $ 
    ScanSizeTop : long(0), $ 
    ScanSizeRight : long(0), $ 
    ScanSizeBottom : long(0), $ 
    FullScanFOVX : double(0), $
    FullScanFOVY : double(0), $
    ; EDX Elemental Maps 
    Element : '                ', $
    EnergyIntervalLower : double(0), $
    EnergyIntervalHigher : double(0), $
    Method : long(0), $
    ; Dose Fractions
    IsDoseFraction : byte(0), $
    FractionNumber : long(0), $
    StartFrame : long(0), $
    EndFrame : long(0), $
    ; Reconstruction 
    InputStackFilename : '->                                                                            <-', $
    BitMask4 : long(0), $
    AlphaTiltMin : double(0), $
    AlphaTiltMax : double(0) $
},numPatterns)

;/**
; * @brief This spec was taken from https://www.ccpem.ac.uk/mrc_format/mrc2014.php
; * and represents the 2014 MRC format definition
; */
MRCHeader = {MRCstruct, $
    nx : long(0), $  ; number of columns in 3D data (fast axis)
    ny : long(0), $  ; number of rows in 3D data (medium axis)
    nz : long(0), $ ; number of sections in 3D data (slow axis)
    mode : long(2), $ ; type of image pixel (2 = 32-bit signed real)
    nxstart : long(0), $ ; location of first column in unit cell
    nystart : long(0), $ ; location of first row in unit cell
    nzstart : long(0), $ ; location of first section in unit cell 
    mx : long(1), $ ; grid size in x
    my : long(1), $ ; grid size in y
    mz : long(1), $ ; grid size in z
    xlen : float(4), $ ; cell size; pixel spacing = xlen/mx, ylen/my, zlen/mz
    ylen : float(4), $
    zlen : float(4), $
    alpha : float(90), $ ; cell angles - ignored by IMOD
    beta : float(90), $
    gamma: float(90), $
    mapc : long(1), $ ; map column  1=x,2=y,3=z
    mapr : long(2), $ ; map row     1=x,2=y,3=z
    maps : long(3), $ ; map section 1=x,2=y,3=z
    dmin : float(1), $ ; minimum pixel value (needs to be set for proper scaling of data)
    dmax : float(0), $ ; maximum pixel value
    dmean : float(-1), $ ; mean pixel value
    ispg : long(1), $ ; space group number
    nsymbt : long(768L*long(numPatterns)), $ ; number of bytes in extended FEI1 header (768 * number of EBSD patterns)
    extra25 : long(0), $  ; first of the words from 25 to 49 
    extra26 : long(0), $  ; second of the words from 25 to 49 
    exttyp : byte('FEI1'),$ ; FEI1 extended header type
    nversion : long(20140),$ ; MRC version number 
    extra29 : long(0), $  ; second of the words from 25 to 49 
    extra30 : long(0), $  ; second of the words from 25 to 49 
    extra31 : long(0), $  ; second of the words from 25 to 49 
    extra32 : long(0), $  ; second of the words from 25 to 49 
    extra33 : long(0), $  ; second of the words from 25 to 49 
    extra34 : long(0), $  ; second of the words from 25 to 49 
    extra35 : long(0), $  ; second of the words from 25 to 49 
    extra36 : long(0), $  ; second of the words from 25 to 49 
    extra37 : long(0), $  ; second of the words from 25 to 49 
    extra38 : long(0), $  ; second of the words from 25 to 49 
    extra39 : long(0), $  ; second of the words from 25 to 49 
    extra40 : long(0), $  ; second of the words from 25 to 49 
    extra41 : long(0), $  ; second of the words from 25 to 49 
    extra42 : long(0), $  ; second of the words from 25 to 49 
    extra43 : long(0), $  ; second of the words from 25 to 49 
    extra44 : long(0), $  ; second of the words from 25 to 49 
    extra45 : long(0), $  ; second of the words from 25 to 49 
    extra46 : long(0), $  ; second of the words from 25 to 49 
    extra47 : long(0), $  ; second of the words from 25 to 49 
    extra48 : long(0), $  ; second of the words from 25 to 49 
    extra49 : long(0), $  ; second of the words from 25 to 49 
    xorg : float(0), $ ; origin of image
    yorg : float(0), $
    zorg : float(0), $
    cmap : 'MAP ', $
    machst: [68B, 68B, 0B, 0B], $ ; First two bytes have 17 and 17 for big-endian or 68 and 68 for little-endian
    rms : float(0), $ ; RMS deviation of densities from mean density
    nLabels : long(0), $ ; Number of labels with useful data
    labels : string(' ',format='(A800)') $ ; 10 labels of 80 characters each
}

end



pro write_mrc,outname

common CBED_data_common, data
common CBEDpattern, CBEDpattern

CBEDprint,'Generating .mrc file ... ',/blank
;
save_thicksel = data.thicksel 

; compute a CBED thickness series and dump it to a .mrc file.
;
dims = size(CBEDpattern,/dimensions)

nth = data.numt
th = data.startthick + findgen(data.numt)*data.thickinc

CBEDstack = fltarr(dims[0],dims[1],data.numt)

for i=0,data.numt-1 do begin 
  data.thicksel = i
  CBEDgocbed
  CBEDstack[0:*,0:*,i] = CBEDpattern
endfor

mi = min(CBEDstack,max=ma)
me = mean(CBEDstack)

; and then convert these slices into a single .mrc file
MRCinit,M,F,nth

; set the M variable
M.nx = long(dims[0])
M.ny = long(dims[1])
M.mx = M.nx
M.my = M.ny
M.nz = long(nth)
M.mz = M.nz
M.dmin = float(mi)
M.dmax = float(ma)
M.dmean = float(me)
M.xlen = float(M.nx)
M.ylen = float(M.ny)
M.zlen = float(M.nz)
M.nLabels = 3
L1 = 'EMsoft IDL implementation, MDG/CMU Copyright 2020'
L2 = 'Incident beam direction: '+'[ '+strtrim(string(data.wavek[0]),2)+','+strtrim(string(data.wavek[1]),2)+','+strtrim(string(data.wavek[2]),2)+' ]'
L3 = 'Horizontal g-vector : '+'( '+strtrim(string(data.ga[0]),2)+','+strtrim(string(data.ga[1]),2)+','+strtrim(string(data.ga[2]),2)+' )'
z1 = ''
for i=strlen(L1),79 do z1 = z1+' '
z2 = ''
for i=strlen(L2),79 do z2 = z2+' '
z3 = ''
for i=strlen(L3),79 do z3 = z3+' '
L = strtrim(L1)+z1+strtrim(L2)+z2+strtrim(L3)
z = ''
for i=strlen(L),799 do z = z + ' '
M.labels = L+z 

; set the parameters 
for i=0,nth-1 do begin
  F[i].HT = data.voltage
  F[i].BitMask1 += 2L^5

  F[i].Defocus = th[i]
  F[i].BitMask1 += 2L^22

  F[i].CameraLength= data.camlen/1000.
  F[i].BitMask2 += 2L^0

  F[i].ConvergenceAngle = data.thetau/1000.0/!dtor
  F[i].BitMask2 += 2L^4

  F[i].PixelSizeX = data.scale 
  F[i].PixelSizey = data.scale 
  F[i].BitMask1 += 2L^14
  F[i].BitMask1 += 2L^15
endfor

; and write the file
spawn,'/usr/bin/touch '+outname
openu,1,outname
writeu,1,M,F
writeu,1,CBEDstack
close,1

; reset original parameters
data.thicksel = save_thicksel
CBEDgocbed 

CBEDprint,'  --> Data stored in '+outname
end
