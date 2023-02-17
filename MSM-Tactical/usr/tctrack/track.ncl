load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_string.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_inv_string.ncl"
begin
  ncols = 7
  tcnum = 14
  plotbst = False
  datadir = "/Volumes/dandelion/GRMSMJob/rsm2msm9_tparc/"
  wks = gsn_open_wks("ps", datadir+"track22"+sprinti("%0.2d",tcnum))

  res = True 
  res@gsnDraw = False 
  res@gsnFrame = False 

  res@mpDataBaseVersion = "RANGS"
  res@mpOutlineOn = True
  res@mpFillOn    = True
  res@mpLandFillColor = "tan"
  res@mpOceanFillColor = "LightCyan"
  res@mpInlandWaterFillColor = res@mpOceanFillColor
  ;res@mpMinLatF = 28.0
  res@mpMinLatF = 5.0
  res@mpMaxLatF = 38.0
  res@mpMinLonF = 120.0
  res@mpMaxLonF = 164.0
  res@gsnRightString = " "
  res@gsnLeftString = " "

  plot = gsn_csm_map_ce(wks, res)
  plres = True 
  pmres = True 

  if (plotbst) then
    tfile = "/Volumes/dandelion/data/tctrack/2022/bst22"+sprinti("%0.2d",tcnum)+".txt"
    plres@gsLineColor = "black"
    plres@gsLineThicknessF = 4.0
    pmres@gsMarkerColor = "black"
    pmres@gsMarkerIndex = 4
    pmres@gsMarkerSizeF = 8.0
    pmres@gsMarkerThicknessF = 4.0
    
    data = readAsciiTable(tfile,ncols,"float",0)
    npoints = dimsizes(data(:,0))
    df = new(npoints, integer)
    hf = new(npoints, integer)
    lonf = new(npoints, float)
    latf = new(npoints, float)
    df(0:npoints-1) = toint(data(:,2))
    hf(0:npoints-1) = toint(data(:,3))
    lonf(0:npoints-1) = data(:,4)
    latf(0:npoints-1) = data(:,5)
    ;dum1 = new(npoints,graphic)
    dum2 = new(npoints,graphic)
    do i = 0, npoints-2
      ;dum1(i) = gsn_add_polyline(wks, plot, (/lonf(i),lonf(i+1)/),(/latf(i),latf(i+1)/),plres)
      dum2(i) = gsn_add_polymarker(wks, plot, lonf(i), latf(i), pmres)
    end do
    dum2(i) = gsn_add_polymarker(wks, plot, lonf(i), latf(i), pmres)
    delete(data)
    delete(plres@gsLineColor)
    delete(pmres@gsMarkerColor)
    delete(df)
    delete(hf)
    delete(lonf)
    delete(latf)
  end if
  npoints = 100
  sdate = "2022091412"
  edate = "2022091712"
  shour = cd_inv_string(sdate,"%Y%N%D%H")
  ehour = cd_inv_string(edate,"%Y%N%D%H")
  dt = 24 ;hours
  nfiles = toint((ehour-shour)/dt) + 1
  ;init = "2022063012"
;  files = (/\
;;  "track2022063012.txt",\
;  "track2022070100.txt",\
;  "track2022070112.txt",\
;  "track2022070200.txt",\
;  "track2022070212.txt",\
;  "track2022070300.txt",\
;  "track2022070312.txt",\
;  "track2022070400.txt",\
;  "track2022070412.txt"\
;  /)
  files = (/\
  datadir+"2022091212/track2022091212.txt",\
  datadir+"2022091312/track2022091312.txt",\
  datadir+"2022091412/track2022091412.txt",\
  datadir+"2022091512/track2022091512.txt",\
  datadir+"2022091612/track2022091612.txt",\
  datadir+"2022091712/track2022091712.txt"\
  /)
  dum3 = new((/npoints, nfiles/), graphic)
  dum4 = new((/npoints, nfiles/), graphic)
  dum5 = new((/npoints, nfiles/), graphic)
  plres@gsLineThicknessF = 4.0
  ;pmres@gsMarkerIndex = 4
  ;pmres@gsMarkerThicknessF = 2.0
  df = new((/npoints, nfiles/), integer)
  hf = new((/npoints, nfiles/), integer)
  lonf = new((/npoints, nfiles/), float)
  latf = new((/npoints, nfiles/), float)
  cmap = read_colormap_file("MPL_viridis")
  ncolors = dimsizes(cmap(:,0))
  color_int = toint(ncolors / (nfiles+1))
  ;colors   = (/"red","blue","green","orange","purple","magenta"/)
  colors = NewList("lifo")
  labels = NewList("lifo")
  patterns = (/    0,     0,      0,       0,       0/)
  markers  = (/   16,    16,     16,      16,      16/)
  marker_seq = (/16,9,7,6/)
  ihour = shour
  do j = 0, nfiles-1
    init = cd_string(ihour,"%Y%N%D%H")
    tfile = datadir+init+"/track"+sprinti("%0.2d",tcnum)+".txt"
    label = "Init:"+cd_string(ihour,"%HZ%D")
    ListPush(labels,(/label/))
    data = readAsciiTable(tfile, ncols, "float", 0)
    print(dimsizes(data))
    mypoints = dimsizes(data(:,0))
    df(0:mypoints-1,j) = toint(data(:,2))
    hf(0:mypoints-1,j) = toint(data(:,3))
    lonf(0:mypoints-1,j) = data(:,4)
    latf(0:mypoints-1,j) = data(:,5)
    ;plres@gsLineColor = colors(j)
    plres@gsLineColor = cmap(j*color_int,:)
    plres@gsLineDashPattern = 0;patterns(j)
    ;pmres@gsMarkerColor = colors(j)
    pmres@gsMarkerColor = cmap(j*color_int,:)
    ListPush(colors,(/cmap(j*color_int,:)/))
    pmres@gsMarkerIndex = 16;markers(j)
    ;k = 0
    do i = 0, mypoints-2
      dum3(i,j) = gsn_add_polyline(wks, plot, (/lonf(i,j),lonf(i+1,j)/),(/latf(i,j),latf(i+1,j)/),plres)
      dum4(i,j) = gsn_add_polymarker(wks, plot, lonf(i,j), latf(i,j), pmres)
    end do
    ;pmres@gsMarkerIndex = marker_seq(k)
    dum4(i,j) = gsn_add_polymarker(wks, plot, lonf(i,j), latf(i,j), pmres)
    ;; edge
    delete(pmres@gsMarkerColor)
    pmres@gsMarkerIndex = 4
    pmres@gsMarkerColor = "black"
    pmres@gsMarkerThicknessF = 2.0
    do i=0,mypoints-1
      dum5(i,j) = gsn_add_polymarker(wks,plot,lonf(i,j),latf(i,j),pmres)
    end do
    delete(pmres@gsMarkerColor)
    delete(data)
    ihour = ihour + dt
  end do

;; legends
  xmin = 0.18
  xwidth = 0.16
  if(nfiles.gt.4) then
  xwidth = 0.32
  end if
  xgon = (/xmin,xmin+xwidth,xmin+xwidth,xmin,xmin/)
  ygon = (/0.9,0.9,0.775,0.775,0.9/)
  xleg = (/0.200,0.200,0.200,0.200,0.350,0.350,0.350,0.350/)
  xtxt = (/0.275,0.275,0.275,0.275,0.425,0.425,0.425,0.425/)
  yleg = (/0.875,0.85,0.825,0.8,0.875,0.85,0.825,0.8/)
  ytxt = yleg
  ;labels = (/"Init:12Z12","Init:12Z13","Init:12Z14","Init:12Z15","Init:12Z16","Init:12Z17"/)
  pgres = True
  pgres@gsFillColor = "gray30"
  pgres@gsFillOpacityF = 0.3
  gsn_polygon_ndc(wks,xgon,ygon,pgres)
  txres = True
  txres@txFontHeightF = 0.015
  pmres = True
  pmres@gsMarkerIndex = 16
  pmres@gsMarkerSizeF = 10.0
  pmres@gsMarkerThicknessF = 3.0
  do i=0,nfiles-1
    pmres@gsMarkerColor = colors[nfiles-1-i]
    ;pmres@gsMarkerColor = cmap(i*color_int,:)
    gsn_polymarker_ndc(wks,xleg(i),yleg(i),pmres)
    gsn_text_ndc(wks,labels[nfiles-1-i],xtxt(i),ytxt(i),txres)
  end do
  draw(plot)
  frame(wks)

end
