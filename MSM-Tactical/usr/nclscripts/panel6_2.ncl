load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_string.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_inv_string.ncl"
begin
  ;init = "2022060600"
  ihour = cd_inv_string(init,"%Y%N%D%H")
  ;ires = 3
  ;fh = 3
  vhour = ihour
  vhour = ihour + fh + 9 ;UTC=>JST
  valid = cd_string(vhour,"%D%HJST")
  print(valid)
  if (ires.eq.9) then
    ;wdir = "rsm2msm9_jpn"
    wdir = "rsm2msm9_tparc"
  else
    ;wdir = "msm2msm3_jpn"
    wdir = "msm2msm3_tparc"
  end if
  a = addfile("/zdata/grmsm/work/"+wdir+"/"+init+"/r_pgb.f"+sprinti("%0.2d",fh), "r")
  dNames = getfiledimnames(a)
  print(dNames)
  vNames = getfilevarnames(a)
  print(vNames)
  lev = a->lv_ISBL2
  lat = a->g1_lat_0
  lon = a->g1_lon_1
  latS = min(lat)
  latN = max(lat)
  lonW = min(lon)
  lonE = max(lon)
  lonC = (lonW+lonE)/2.
  print(lev)
  ;; common
  z = a->HGT_GDS1_ISBL(:,:,:)
  t = a->TMP_GDS1_ISBL(:,:,:)
  u = a->U_GRD_GDS1_ISBL(:,:,:)
  v = a->V_GRD_GDS1_ISBL(:,:,:)
  u = u / 0.5144
  u@units = "kt"
  v = v / 0.5144
  v@units = "kt"
  printVarSummary(z)
  printMinMax(z,0)
  printVarSummary(t)
  printMinMax(t,0)
  printVarSummary(u)
  printMinMax(u,0)
  printVarSummary(v)
  printMinMax(v,0)
  ;; 300hPa
  wspd = u({300},:,:)
  wspd = sqrt(u({300},:,:)^2 + v({300},:,:)^2)
  wspd@long_name = "Wind speed"
  printVarSummary(wspd)
  printMinMax(wspd,0)
  ;; 500hPa
  avor = a->ABS_V_GDS1_ISBL({500},:,:)
  vor = avor
  printVarSummary(avor)
  printMinMax(avor,0)
  fcori = coriolis_param(lat)
  vor = vor - conform(vor,fcori,0)
  vor@long_name = "Relative Vorticity"
  vor = vor * 1.0e6
  vor@units = "10^-6/s"
  printVarSummary(vor)
  printMinMax(vor,0)
  ;; 700hPa
  omg = a->V_VEL_GDS1_ISBL({700},:,:)
  omg = omg * 1.0e-2 * 3600.0
  omg@units = "hPa/h"
  printVarSummary(omg)
  printMinMax(omg,0) 
  ;; 850hPa
  q = a->SPF_H_GDS1_ISBL(:,:,:)
  ;printVarSummary(q)
  ;printMinMax(q,0)  
  plev = lev*1.0e2
  plev@units = "Pa"
  te = pot_temp_equiv(plev,t,q,0,"q")
  printVarSummary(te)
  printMinMax(te,0)
  ;; surface
  psea = a->PRMSL_GDS1_MSL(:,:)
  u10  = a->U_GRD_GDS1_HTGL(:,:)
  v10  = a->V_GRD_GDS1_HTGL(:,:)
  psea = psea*1.0e-2
  psea@units = "hPa"
  u10 = u10 / 0.5144
  u10@units = "kt"
  v10 = v10 / 0.5144
  v10@units = "kt"
  printVarSummary(psea)
  printMinMax(psea,0)
  printVarSummary(u10)
  printMinMax(u10,0)
  printVarSummary(v10)
  printMinMax(v10,0)
  if (fh.ge.3) then
  ;r1h  = a->A_PCP_GDS1_SFC_acc1h(:,:)
  ;r3h = r1h
  ;atmp = addfile("/zdata/grmsm/work/"+wdir+"/"+init+"/r_pgb.f"+sprinti("%0.2d",fh-1), "r")
  ;r1h  = a->A_PCP_GDS1_SFC_acc1h(:,:)
  ;r3h = r3h + r1h
  ;delete(atmp)
  ;atmp = addfile("/zdata/grmsm/work/"+wdir+"/"+init+"/r_pgb.f"+sprinti("%0.2d",fh-2), "r")
  ;r1h  = a->A_PCP_GDS1_SFC_acc1h(:,:)
  ;r3h = r3h + r1h
  ;delete(atmp)
  r3h  = a->A_PCP_GDS1_SFC_acc3h(:,:)
  printVarSummary(r3h)
  printMinMax(r3h,0)
  end if
  ;; SST
  slmsk = a->LAND_GDS1_SFC(:,:)
  tsfc  = a->TMP_GDS1_SFC(:,:)
  tsea  = tsfc
  tsea  = tsea - 273.15
  tsea  = mask(tsea,slmsk,0.)
  tsea@long_name = "Sea surface temperature"
  tsea@units     = "degreeC"
  printVarSummary(tsea)
  printMinMax(tsea,0)
  ;; 950hPa v=0
  v950 = v({950},:,:)
  gradLatLon = grad_latlon_cfd(v950,v950&g1_lat_0,v950&g1_lon_1,False,False)
  dvdy = gradLatLon[0]
  ;dvdx = gradlatLon[1] ;; not used
  ;printVarSummary(dvdy)
  v0mask = new(dimsizes(dvdy),integer)
  v0mask = where(dvdy.lt.0, 1, 0)
  ;printVarSummary(v0mask)
  v0 = v950
  v0 = mask(v0,v0mask,1)
  printVarSummary(v0)
  printMinMax(v0,0)
  v0color = "magenta"
  ;; plot
  plots = new(6,graphic)
  wks = gsn_open_wks("png","panel6_fh"+sprinti("%0.3d",fh))
  ;; plot(0)=300hPa ISTC(kt), Z(m), T(degreeC), Windbarb 
  ;; plot(1)=500hPa VOR(10^-6/s), Z(m), Windbarb 
  ;; plot(2)=700hPa OMG(hPa/h), T(degreeC), Windbarb 
  ;; plot(3)=850hPa EPT(K), Windbarb 
  ;; plot(4)=surface R3H(mm), PSEA(hPa), Windbarb 
  ;; plot(5)=SST(degreeC), 950hPa Windbarb, V=0 line 
  t = t - 273.15
  t@units = "degreeC"
  res = True
  res@gsnDraw = False
  res@gsnFrame = False
  res@gsnMaximize = True
  res@gsnAddCyclic = False
  res@gsnLeftString = ""
  res@gsnRightString = ""

  resc = res ; contour only
  resc@cnInfoLabelOn = False
  
  res@mpProjection = "Mercator"
  res@mpLimitMode = "LatLon"
  res@mpDataBaseVersion = "HighRes"
  res@mpFillOn = False
  res@mpMaxLatF = latN
  res@mpMinLatF = latS
  res@mpMaxLonF = lonE
  res@mpMinLonF = lonW
  res@mpCenterLonF = lonC
  res@vcRefMagnitudeF = 10.
  res@vcRefLengthF = 0.025
  res@vcMinDistanceF = 0.05
  res@vcGlyphStyle = "WindBarb"
  res@gsnScalarContour = True
  res@vcRefAnnoOn = False
  res@lbOrientation = "vertical"
  res0 = res
  res1 = res
  res2 = res
  res3 = res
  res4 = res
  res5 = res
  res0@cnFillOn = True
  res0@cnLinesOn = False
  cmap0 = read_colormap_file("WhiteBlueGreenYellowRed")
  newcmap0 = cmap0(:145,:)
  res0@cnFillPalette = newcmap0
  res0@cnLevelSelectionMode = "ManualLevels"
  res0@cnMinLevelValF = 60.0
  res0@cnMaxLevelValF = 140.0
  res0@cnLevelSpacingF = 20.0
  res0@gsnCenterString = "300hPa ISTC(kt), Z(m), T(~S~o~N~C), Wind(kt)"
  plots(0) = gsn_csm_vector_scalar_map(wks, u({300},:,:), v({300},:,:), wspd, res0)
  resc@cnFillOn = False
  resc@cnLinesOn = True
  resc@cnLineColor = "black"
  resc@cnLevelSelectionMode = "ManualLevels"
  resc@cnMinLevelValF = 9000.0
  resc@cnMaxLevelValF = 9900.0
  resc@cnLevelSpacingF = 60.0
  resc@cnLineLabelInterval = 5
  resc@cnMonoLineThickness = False
  resc@cnLineThicknesses = (/4.0,2.0,2.0,2.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0/)
  plot0c = gsn_csm_contour(wks,z({300},:,:),resc)
  overlay(plots(0),plot0c)
  resc@cnLineColor = "red"
  resc@cnMinLevelValF = -51.0
  resc@cnMaxLevelValF = -27.0
  resc@cnLevelSpacingF = 1.0
  resc@cnLineLabelInterval = 3
  delete(resc@cnLineThicknesses)
  resc@cnLineThicknesses = (/4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0/)
  plot0c2 = gsn_csm_contour(wks,t({300},:,:),resc)
  overlay(plots(0),plot0c2)

  res1@cnFillOn = True
  res1@cnLinesOn = False
  cmap1 = read_colormap_file("MPL_BrBG")
  newcmap1 = cmap1(:65:-1,:)
  res1@cnFillPalette = newcmap1
  res1@cnLevelSelectionMode = "ManualLevels"
  res1@cnMinLevelValF = 0.0
  res1@cnMaxLevelValF = 120.0
  res1@cnLevelSpacingF = 40.0
  res1@gsnCenterString = "500hPa Vor(10~S~-6~N~/s), Z(m), Wind(kt)"
  plots(1) = gsn_csm_vector_scalar_map(wks,u({500},:,:),v({500},:,:),vor,res1)
  resc@cnFillOn = False
  resc@cnLinesOn = True
  resc@cnLineColor = "black"
  resc@cnLevelSelectionMode = "ManualLevels"
  resc@cnMinLevelValF = 5500.0
  resc@cnMaxLevelValF = 5900.0
  resc@cnLevelSpacingF = 20.0
  resc@cnLineLabelInterval = 5
  resc@cnMonoLineThickness = False
  delete(resc@cnLineThicknesses)
  resc@cnLineThicknesses = (/4.0,2.0,2.0,2.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0/)
  plot1c = gsn_csm_contour(wks,z({500},:,:),resc)
  overlay(plots(1),plot1c)

  res2@cnFillOn = True
  res2@cnLinesOn = False
  cmap2 = read_colormap_file("GMT_red2green")
  newcmap2 = cmap2(:9,:)
  newcmap2(9,:2) = (/1.0,1.0,1.0/)
  res2@cnFillPalette = newcmap2
  res2@cnLevelSelectionMode = "ExplicitLevels"
  res2@cnLevels = (/-70.0,-30.0,-10.0,0.0/)
  ;res2@gsnCenterString = "700hPa OMG(hPa/h), T(~S~o~N~C), Wind(kt)"
  res2@gsnCenterString = "700hPa OMG(hPa/h), Wind(kt)"
  plots(2) = gsn_csm_vector_scalar_map(wks,u({700},:,:),v({700},:,:),omg,res2)
  resc@cnLineColor = "red"
  resc@cnMinLevelValF = -6.0
  resc@cnMaxLevelValF = 18.0
  resc@cnLevelSpacingF = 1.0
  resc@cnLineLabelInterval = 3
  delete(resc@cnLineThicknesses)
  resc@cnLineThicknesses = (/4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0/)
  ;plot2c = gsn_csm_contour(wks,t({700},:,:),resc)
  ;overlay(plots(2),plot2c)

  res3@cnFillOn = True
  res3@cnLinesOn = False
  cmap3 = read_colormap_file("BlAqGrYeOrReVi200")
  newcmap3 = new((/184,4/),float)
  newcmap3(:113,:) = cmap3(:113,:)
  newcmap3(114:,:) = cmap3(130:,:)
  res3@cnFillPalette = newcmap3
  res3@cnLevelSelectionMode = "ManualLevels"
  res3@cnMinLevelValF = 280.0
  res3@cnMaxLevelValF = 370.0
  res3@cnLevelSpacingF = 2.0
  res3@lbLabelAutoStride = False
  res3@lbLabelStride = 5
  res3@lbBoxLinesOn = False
  res3@gsnCenterString = "850hPa EPT(K), Wind(kt)"
  plots(3) = gsn_csm_vector_scalar_map(wks,u({850},:,:),v({850},:,:),te({850},:,:),res3)
  resc@cnFillOn = False
  resc@cnLinesOn = True
  resc@cnLineColor = "gray"
  resc@cnLevelSelectionMode = "ManualLevels"
  resc@cnMinLevelValF = 282.0
  resc@cnMaxLevelValF = 363.0
  resc@cnLevelSpacingF = 3.0
  delete(resc@cnLineThicknesses)
  resc@cnLineThicknesses = (/4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0,2.0,2.0,\
                             4.0,2.0,2.0,4.0/)
  plot3c = gsn_csm_contour(wks,te({850},:,:),resc)
  overlay(plots(3),plot3c)

  if (fh.ge.3) then
  res4@cnFillOn = True
  res4@cnLinesOn = False
  colors = (/"white","lightblue","cyan","limegreen",\
             "yellow","orange","red","lightpink"/)
  newcmap4 = namedcolor2rgba(colors)
  res4@cnFillPalette = newcmap4
  res4@cnLevelSelectionMode = "ExplicitLevels"
  res4@cnLevels = (/0.4,1.0,5.0,10.0,20.0,50.0,100.0/)
  res4@gsnCenterString = "SURF R3H(mm) PSEA(hPa), Wind(kt)"
  plots(4) = gsn_csm_vector_scalar_map(wks,u10,v10,r3h,res4)
  resc@cnFillOn = False
  resc@cnLinesOn = True
  resc@cnLineColor = "black"
  ;resc@cnLevelSelectionMode = "ManualLevels"
  ;resc@cnMinLevelValF = 990.0
  ;resc@cnMaxLevelValF = 1020.0
  ;resc@cnLevelSpacingF = 2.0
  resc@cnLevelSelectionMode = "ExplicitLevels"
  resc@cnLevels = (/ 965., 970., 975., 980., 985.,\
                     990., 992., 994., 996., 998.,\
                    1000.,1002.,1004.,1006.,1008.,\
                    1010.,1012.,1014.,1016.,1018.,\
                    1020./)
  resc@cnLineLabelInterval = 5
  resc@cnMonoLineThickness = False
  delete(resc@cnLineThicknesses)
  resc@cnLineThicknesses = (/2.0,4.0,2.0,4.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0/)
  plot4c = gsn_csm_contour(wks,psea,resc)
  overlay(plots(4),plot4c)
  else
  res4@cnFillOn = False
  res4@cnLinesOn = True
  res4@cnLineColor = "black"
  ;res4@cnLevelSelectionMode = "ManualLevels"
  ;res4@cnMinLevelValF = 980.0
  ;res4@cnMaxLevelValF = 1020.0
  ;res4@cnLevelSpacingF = 2.0
  res4@cnLevelSelectionMode = "ExplicitLevels"
  res4@cnLevels = (/ 965., 970., 975., 980., 985.,\
                     990., 992., 994., 996., 998.,\
                    1000.,1002.,1004.,1006.,1008.,\
                    1010.,1012.,1014.,1016.,1018.,\
                    1020./)
  res4@cnLineLabelInterval = 5
  res4@cnMonoLineThickness = False
  res4@cnLineThicknesses = (/2.0,4.0,2.0,4.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0,2.0,2.0,2.0,2.0,\
                             4.0/)
  res4@gsnCenterString = "SURF PSEA(hPa), Wind(kt)"
  plots(4) = gsn_csm_vector_scalar_map(wks,u10,v10,psea,res4)
  end if

  res5@cnFillOn = True
  res5@cnLinesOn = False
  cmap5 = read_colormap_file("cmp_b2r")
  res5@cnFillPalette = cmap5
  res5@cnLevelSelectionMode = "ManualLevels"
  res5@cnMinLevelValF = 18.0
  res5@cnMaxLevelValF = 30.0
  res5@cnLevelSpacingF = .5
  res5@lbLabelAutoStride = False
  res5@lbLabelStride = 2
  res5@lbBoxLinesOn = False
  res5@gsnCenterString = "SST(~S~o~N~C), 950hPa Wind(kt)"
  plots(5) = gsn_csm_vector_scalar_map(wks,u({950},:,:),v({950},:,:),tsea(:,:),res5)
  ;;; v0 line
  ;resc@cnFillOn = False
  ;resc@cnLinesOn = True
  ;resc@cnLineColor = v0color
  ;resc@cnLevelSelectionMode = "ExplicitLevels"
  ;resc@cnLevels = (/0.0/)
  ;delete(resc@cnLineThicknesses)
  ;resc@cnMonoLineThickness = True
  ;resc@cnLineThicknessF = 5.0
  ;;overlay(plots(5),plotv0)
  ;do k=0,5
  ;  plotv0 = gsn_csm_contour(wks,v0(:,:),resc)
  ;  overlay(plots(k),plotv0)
  ;  delete(plotv0)
  ;end do
  ;;
  resP = True
  resP@gsnPanelMainString = z@initial_time + " FT="+sprinti("%d",fh)+"H Valid="+valid ;+"  megenta:V950=0"
  gsn_panel(wks,plots,(/2,3/),resP)
end
