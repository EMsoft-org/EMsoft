function ECCIblur,im,blur

  dim = round(6.0*blur)
; make sure filter has odd size and is at least 3 pixels
  if (dim mod 2 eq 0) then dim=dim+1
  if (dim lt 3) then dim=3
  d2 = (dim-1)/2

; some useful auxiliary variables
  kernel = fltarr(dim,dim)
  line = findgen(dim)-float(dim/2)
  x = line#replicate(1.0,dim)
  y = rotate(x,3)
  r = x^2+y^2
  kernel = exp(-r*0.5/blur^2)/(2.0*!pi*blur^2)
  slice = convol(im,kernel,/edge_truncate)

return,slice
end
