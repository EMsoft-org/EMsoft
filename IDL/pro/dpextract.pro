pro dpextract, hdfname

; this routine takes an HDF 5 dot product file and extracts all the images from it

file_id = H5F_OPEN(hdfname+'.h5')
group_id = H5G_OPEN(file_id,'Scan 1/EBSD/Data')
dset_id = H5D_OPEN(group_id,'CIMap')
CImap = H5D_READ(dset_id)
H5D_close,dset_id
dset_id = H5D_OPEN(group_id,'AvDotProductMap')
ADPmap = H5D_READ(dset_id)
H5D_close,dset_id
dset_id = H5D_OPEN(group_id,'IQMap')
IQmap = H5D_READ(dset_id)
H5D_close,dset_id
dset_id = H5D_OPEN(group_id,'KAM')
KAMmap = H5D_READ(dset_id)
H5D_close,dset_id
dset_id = H5D_OPEN(group_id,'OSM')
OSMmap = H5D_READ(dset_id)
H5D_close,dset_id
dset_id = H5D_OPEN(group_id,'Phi')
phi = H5D_READ(dset_id)
H5D_close,dset_id
dset_id = H5D_OPEN(group_id,'Phi1')
phi1 = H5D_READ(dset_id)
H5D_close,dset_id
dset_id = H5D_OPEN(group_id,'Phi2')
phi2 = H5D_READ(dset_id)
H5D_close,dset_id
H5G_close,group_id
H5F_close,file_id

write_jpeg,hdfname+'_CI.jpeg',reverse(bytscl(CImap),2),quality=100
write_jpeg,hdfname+'_ADP.jpeg',reverse(bytscl(ADPmap),2),quality=100
write_jpeg,hdfname+'_IQ.jpeg',reverse(bytscl(IQmap),2),quality=100
write_jpeg,hdfname+'_KAM.jpeg',reverse(bytscl(KAMmap),2),quality=100
write_jpeg,hdfname+'_OSM.jpeg',reverse(bytscl(OSMmap),2),quality=100

sz = size(OSMmap,/dimensions)
nump = sz[0]*sz[1]
Phi = reform(Phi[0:nump-1],sz[0],sz[1])
Phi1= reform(Phi1[0:nump-1],sz[0],sz[1])
Phi2= reform(Phi2[0:nump-1],sz[0],sz[1])

c = bytarr(3,sz[0],sz[1])
c[0,0:*,0:*] = reverse(bytscl(Phi1,min=0.0,max=2.0*!pi),2)
c[1,0:*,0:*] = reverse(bytscl(Phi ,min=0.0,max=1.0*!pi),2)
c[2,0:*,0:*] = reverse(bytscl(Phi2,min=0.0,max=2.0*!pi),2)
write_jpeg,hdfname+'_falsecolor.jpeg',c,true=1,quality=100

end
