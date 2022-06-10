load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_string.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_inv_string.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
begin
  idate = "2022052000"
  edate = "2022053112"
  dt = 12 ; hours
  ihour = cd_inv_string(idate,"%Y%N%D%H")
  ehour = cd_inv_string(edate,"%Y%N%D%H")
  ndate = (toint(ehour) - toint(ihour))/dt + 1
  print(ndate)
  ires = 3
  level = 500
  latS = 24.0
  latN = 33.0
  lonW = 121.0
  lonE = 134.0
  pi = acos(-1.0)
  deg2rad = pi / 180.0
  if (ires.eq.9) then
    wdir="rsm2msm9_jpn"
  else
    wdir="msm2msm3_jpn"
  end if
  wgtFileName0 = "NCEP_2_0.1deg.nc"
  wgtFileName1 = "JMA_2_0.1deg.nc"
  ofile = "rmse_z"+sprinti("%d",level)+"_res"+sprinti("%d",ires)+"_"+idate+"-"+edate+".csv"
  print(ofile)
  system("rm "+ofile)
  do fh=0, 48, 3
  init = idate
  chour = ihour
  rmse_avg = 0.0
  rmse_avg00 = 0.0
  rmse_avg12 = 0.0
  rmse_std = 0.0
  ;acc_avg = 0.0
  ;acc_std = 0.0
  do it = 0, ndate-1
  vhour = chour
  vhour = chour + fh
  valid = cd_string(vhour,"%Y%N%D%H")
  print(valid)
  ;; NCEP MSM forecast
  a = addfile("/zdata/grmsm/work/"+wdir+"/"+init+"/r_pgb.f"+sprinti("%0.2d",fh), "r")
  ;dNames = getfiledimnames(a)
  ;print(dNames)
  ;vNames = getfilevarnames(a)
  ;print(vNames)
  lat = a->g1_lat_0
  lon = a->g1_lon_1
  lat0 = min(lat)
  lat1 = max(lat)
  lon0 = min(lon)
  lon1 = max(lon)
  z = a->HGT_GDS1_ISBL({level},:,:)
  printVarSummary(z)
  printMinMax(z,0)
  ;; JMA MSM analysis
  yyyy = str_get_cols(valid,0,3)
  mm   = str_get_cols(valid,4,5)
  dd   = str_get_cols(valid,6,7)
  ihh  = toint(str_get_cols(valid,8,9))
  ft   = ihh / 3 
  ncfile = "/zdata/jmagpv/msm/"+yyyy+"/anl/P"+mm+dd+".nc"
  print(ncfile)
  b = addfile(ncfile, "r")
  print(b->time(ft))
  za= dble2flt(b->z(ft,{level},:,:))
  lata= b->lat
  lona= b->lon
  printVarSummary(za)
  printMinMax(za,0)
  lata0 = min(lata)
  lata1 = max(lata)
  lona0 = min(lona)
  lona1 = max(lona)
  lat0 = max((/lat0,lata0/))
  lat1 = min((/lat1,lata1/))
  lon0 = max((/lon0,lona0/))
  lon1 = min((/lon1,lona1/))
  ;print(lat0)
  ;print(lat1)
  ;print(lon0)
  ;print(lon1)
  if ((it.eq.0).and.(fh.eq.0)) then
  ;; Convert NCEP MSM to SCRIP Convention file.
  ;; output files for regrid
  srcGridName0 = "src_NCEP_SCRIP.nc"
  Opt = True
  Opt@ForceOverwrite = True
  Opt@PrintTiming = True
  Opt@Title = "NCEP Grid"
  Opt@Mask2D = where(ismissing(z),0,1)
  rectilinear_to_SCRIP(srcGridName0,z&g1_lat_0,z&g1_lon_1,Opt)
  delete(Opt)
  ;; Convert JMA MSM to SCRIP Convention file.
  ;; output files for regrid
  srcGridName1 = "src_JMA_SCRIP.nc"
  Opt = True
  Opt@ForceOverwrite = True
  Opt@PrintTiming = True
  Opt@Title = "JMA Grid"
  Opt@Mask2D = where(ismissing(za),0,1)
  rectilinear_to_SCRIP(srcGridName1,za&lat,za&lon,Opt)
  delete(Opt)
  ;; Convert destination grid to a SCRIP convention file.
  dstGridName = "dst_0.1deg_SCRIP.nc"
  Opt = True
  Opt@ForceOverwrite = True
  Opt@PrintTiming = True
  Opt@Title = "0.1deg Grid"
  Opt@LLCorner = (/latS,lonW/)
  Opt@URCorner = (/latN,lonE/)
  latlon_to_SCRIP(dstGridName,"0.1deg",Opt)
  delete(Opt)
  ;; Generate weights
  Opt = True
  Opt@WgtFileName = wgtFileName0
  Opt@InterpMethod = "bilinear"
  Opt@SrcRegional = True
  Opt@DstRegional = True
  Opt@ForceOverwrite = True
  Opt@PrintTiming = True
  Opt@Debug = True
  ESMF_regrid_gen_weights(srcGridName0,dstGridName,wgtFileName0,Opt)
  delete(Opt)
  Opt = True
  Opt@WgtFileName = wgtFileName1
  Opt@InterpMethod = "bilinear"
  Opt@SrcRegional = True
  Opt@DstRegional = True
  Opt@ForceOverwrite = True
  Opt@PrintTiming = True
  Opt@Debug = True
  ESMF_regrid_gen_weights(srcGridName1,dstGridName,wgtFileName1,Opt)
  delete(Opt)
  end if
  ;; Apply weights
  Opt = True
  Opt@Debug = True
  Opt@PrintTiming = True
  Opt@DstGridType = "rectilinear"
  z_regrid = ESMF_regrid_with_weights(z,wgtFileName0,Opt)
  delete(Opt)
  Opt = True
  Opt@Debug = True
  Opt@PrintTiming = True
  Opt@DstGridType = "rectilinear"
  za_regrid = ESMF_regrid_with_weights(za,wgtFileName1,Opt)
  delete(Opt)
  printVarSummary(z_regrid)
  printVarSummary(za_regrid)
  ;; Calculate RMSE
  rmse = wgt_arearmse(z_regrid,za_regrid,cos(z_regrid&lat*deg2rad),1.0,0)
  print(rmse)
  rmse_avg = rmse_avg + rmse
  if (it%2 .eq. 0) then
    rmse_avg00 = rmse_avg00 + rmse
  else
    rmse_avg12 = rmse_avg12 + rmse
  end if
  rmse_std = rmse_std + rmse^2
  ;; Time cycle
  chour = chour + dt
  init = cd_string(chour,"%Y%N%D%H")
  print(init)
  end do
  rmse_avg = rmse_avg / ndate
  rmse_avg00 = rmse_avg00 / ndate * 2
  rmse_avg12 = rmse_avg12 / ndate * 2
  rmse_std = sqrt(rmse_std / ndate - rmse_avg^2)
  print(rmse_avg)
  print(rmse_avg00)
  print(rmse_avg12)
  print(rmse_std)
  write_table(ofile,"a",[/fh,rmse_avg,rmse_avg00,rmse_avg12,rmse_std/],\
              "%2i,%9.6f,%9.6f,%9.6f,%9.6f")
  end do
  system("cat "+ofile)
end
