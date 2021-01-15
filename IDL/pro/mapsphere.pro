;
; Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:mapsphere.pro
;--------------------------------------------------------------------------
;
; PROGRAM: mapsphere.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief draw a master pattern on a sphere
;
;> @date 02/27/14 MDG 1.0 first version
;> @date 05/04/17 MDG 1.1 updated for hdf5 master pattern file
;--------------------------------------------------------------------------
pro mapsphere,hdfname
;

file_id = H5F_OPEN(hdfname)
group_id = H5G_OPEN(file_id,'EMData/EBSDmaster')
dset_id = H5D_OPEN(group_id,'mLPNH')
image = H5D_READ(dset_id)
H5D_close,dset_id
H5G_close,group_id
H5F_close,file_id

; sum over all atom positions and all energies
sz = size(image)
if (sz[0] eq 3) then image = total(image,3)
if (sz[0] eq 4) then image = total(total(image,4),3)

; scale to 0-255 range
image -= min(image)
image /= max(image)
image *= 255.0

; get teh size
sz = size(image,/dimensions)
nx = sz[0]
ny = sz[1]

; Creating a sphere with a constant radius to use as the data sphere.
nump = 801
MESH_OBJ, 4, vertices, polygons, REPLICATE(1.0, nump, nump)

; next we need to establish the vertex mapping for the modified Lambert projection
units = vertices	; unit vectors to vertices
np = size(units,/dimensions)
np = np[1]
for i=0L,np-1L do begin
 n = norm(units[0:2,i])
 units[0:2,i] /= n
endfor

; count the points in the Southern hemisphere
vz = reform(vertices[2,*])
q=where(vz lt 0,cnt)
print,cnt,' points in Southern hemisphere'

; get the reverse mapping
units[0:2,q] = -units[0:2,q]

XX = reform(units[0,*])
YY = reform(units[1,*])
ZZ = reform(units[2,*])

; first the square to circle mapping using the bilinear function.
; we need two arrays that contain for each point in the circle map
; the equivalent point in the square Lambert map.
xmap = fltarr(nx,ny)
ymap = fltarr(nx,ny)
nx2 = (nx-1)/2
ny2 = (ny-1)/2

Rmax = float(nx2)^2
line = findgen(nx)-(nx-1)/2
xsquare = line # replicate(1,nx)
ysquare = replicate(1,ny) # line
xcircle = fltarr(nx,ny)
ycircle = fltarr(nx,ny)

sp2 = sqrt(!pi)/2.0
spi = 1.0/sp2
for i=0,nx-1 do begin
  A = float(i-nx2)
  for j=0,ny-1 do begin
    B = float(j-ny2)
    if (A^2+B^2 le Rmax) then begin
      if ( (0 le abs(B)) and (abs(B) le abs(A)) and (abs(A)+abs(B) ne 0) ) then begin
        xcircle[i,j] = (A/abs(A)) * sqrt(A^2+B^2) * sp2
        ycircle[i,j] = (A/abs(A)) * sqrt(A^2+B^2) * spi * atan(B/A)
      end

      if ( (0 le abs(A)) and (abs(A) le abs(B)) and (abs(A)+abs(B) ne 0) ) then begin
        xcircle[i,j] = (B/abs(B)) * sqrt(A^2+B^2) * spi * atan(A/B)
        ycircle[i,j] = (B/abs(B)) * sqrt(A^2+B^2) * sp2
      end

    endif
  endfor
endfor

; scale
xcircle *= spi
ycircle *= spi

; and shift
xcircle += nx2
ycircle += ny2

; then do bilinear interpolation and show the resulting array
circle = bilinear(image,xcircle,ycircle,missing = 0)
window,0,xsi=nx,ysi=ny,retain=2
tvscl,circle

; next, use inverse modified Lambert map to get this onto the sphere surface.
x = replicate(0.0,np)
y = replicate(0.0,np)

; eq (10) from Rosca's paper
q = where( (0.0 le abs(YY)) and (abs(YY) le abs(XX)) and (abs(XX)+abs(YY) ne 0.0) )
x[q] = XX[q]/abs(XX[q]) * sqrt(2.0*(1.0-ZZ[q])) * sp2
y[q] = XX[q]/abs(XX[q]) * sqrt(2.0*(1.0-ZZ[q])) * spi * atan(YY[q]/XX[q])

; eq (11)
q = where( (0.0 le abs(XX)) and (abs(XX) le abs(YY)) and (abs(XX)+abs(YY) ne 0.0) )
x[q] = YY[q]/abs(YY[q]) * sqrt(2.0*(1.0-ZZ[q])) * spi * atan(XX[q]/YY[q])
y[q] = YY[q]/abs(YY[q]) * sqrt(2.0*(1.0-ZZ[q])) * sp2

; deal with the origin
q = where( (XX eq 0.0) and (YY eq 0.0), cnt )
if (cnt gt 0) then begin
  x[q] = 0.0
  y[q] = 0.0
endif

; get min and max values
minx = min(x,max=maxx)
miny = min(y,max=maxy)

; rescale to [0,1] square
x = (x-minx)/(maxx-minx)
y = (y-miny)/(maxy-miny)

; next we transform these into the texture coordinates 
texture_coordinates = fltarr(2,np)
texture_coordinates[0,0:*] = x[0:*]
texture_coordinates[1,0:*] = y[0:*]

; Creating a model object to contain the display.
oModel = OBJ_NEW('IDLgrModel')

; Creating image and palette objects to contain the
; imported image and color table.
oPalette = OBJ_NEW('IDLgrPalette')
oPalette -> LoadCT, 00
oPalette -> SetRGB, 255,255,255,255
oImage = OBJ_NEW('IDLgrImage', image, PALETTE = oPalette)

; Creating the polygon object containing the data.
oPolygons = OBJ_NEW('IDLgrPolygon', SHADING = 1, $
	DATA = vertices, POLYGONS = polygons, $
	COLOR = [255,255,255], $
	TEXTURE_COORD = texture_coordinates, $
	TEXTURE_MAP = oImage, /TEXTURE_INTERP)

; Adding polygon to model container.  NOTE:  the polygon
; object already contains the texture map image and its
; related palette.
oModel -> ADD, oPolygons

; Rotating model to display zero degrees latitude and
; zero degrees longitude as front.
oModel -> ROTATE, [1, 0, 0], -90
oModel -> ROTATE, [0, 1, 0], -90

; Displaying results.
XOBJVIEW, oModel, /BLOCK

; Cleaning up object references.
OBJ_DESTROY, [oModel, oImage, oPalette]

END

