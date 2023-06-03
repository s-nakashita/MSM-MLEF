load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_string.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_inv_string.ncl"
begin
  ncols = 7
  tcnum = 11
  plotbst = True
  datadir = "/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work/rsm2msm9/"
  wks = gsn_open_wks("x11", "slpc22"+sprinti("%0.2d",tcnum))
  ;wks = gsn_open_wks("png", "slpc2214")

  res = True 
  res@gsnDraw = False 
  res@gsnFrame = False 
  ;res@tiMainString = "Best Track of Hagibis (2019)"

  res@vpHeightF = 0.4
  res@vpWidthF = 0.8
  res@vpXF = 0.15
  res@vpYF = 0.75

  res@trYMinF = 940.0
  res@trYMaxF = 1010.0
  res@trXMinF = -3.
  res@trXMaxF = 195.

  res@tiYAxisString = "hPa"
  ;res@tiXAxisString = "hours since 2022-09-14 06:00:00"
  res@tmXBMode = "Explicit"
  res@tmXBValues = ispan(0,192,3)
  res@tmXBLabels = ispan(0,192,3)
  res@tmLabelAutoStride = True
  res@tmXMajorGrid = True
  res@tmXMajorGridThicknessF = 1.0
  res@tmXMajorGridLineDashPattern = 1
  res@tmXMajorGridLineColor = "gray30"
  res@gsnRightString = " "
  res@gsnLeftString = " "

  npoints = 100
  sdate = "2022082800"
  edate = "2022083100"
  shour = cd_inv_string(sdate,"%Y%N%D%H")
  ehour = cd_inv_string(edate,"%Y%N%D%H")
  dt = 24 ;hours
  nfiles = toint((ehour-shour)/dt) + 1
  tbase = cd_string(shour,"%Y-%N-%D %H:%M:%S")
  res@tiXAxisString = "hours since "+tbase

  if (plotbst) then
    tfile = "/Users/nakashita/mnt/dandelion/data/tctrack/2022/bst22"+sprinti("%0.2d",tcnum)+".txt"
    res@xyLineColor = "black"
    res@xyLineThicknessF = 6.0

    data = readAsciiTable(tfile,ncols,"float",0)
    npoints = dimsizes(data(:,0))
    nn = ispan(0, npoints-1, 1) * 0.0
    ss = ispan(0, npoints-1, 1) * 0.0
    date = cd_inv_calendar( data(:,0), data(:,1), data(:,2), data(:,3), nn, ss, \
            res@tiXAxisString, 0)
    print(date)
    slpc = new(npoints, float)
    slpc!0 = "time"
    slpc&time = date 
    slpc(:) = data(:,6) * 0.01
    print(slpc)
    res@xyDashPattern = 0
    plot = gsn_csm_xy(wks, date, slpc, res)
    delete(data)
    delete(nn)
    delete(ss)
    delete(date)
    delete(slpc)
    delete(res@xyLineColor)
  end if
;  cmap = read_colormap_file("MPL_viridis")
  cmap = read_colormap_file("grads_rainbow")
  ncolors = dimsizes(cmap(:,0))
  color_int = toint(ncolors / (nfiles+1))
  ;colors   = (/"red","blue","green","orange","purple","magenta"/)
  colors = NewList("lifo")
  labels = NewList("lifo")
  res@xyLineThicknessF = 6.0
  plots = new(nfiles, graphic)
  ihour = shour
  do j = 0, nfiles-1
    init = cd_string(ihour,"%Y%N%D%H")
    tfile = datadir+init+"/track"+sprinti("%0.2d",tcnum)+".txt"
    label = "Init:"+cd_string(ihour,"%HZ%D")
    ListPush(labels,(/label/))
    data = readAsciiTable(tfile, ncols, "float", 0)
    mypoints = dimsizes(data(:,0))
    nn = ispan(0, mypoints-1, 1) * 0.0
    ss = ispan(0, mypoints-1, 1) * 0.0
    date = cd_inv_calendar( data(:,0), data(:,1), data(:,2), data(:,3), nn, ss, \
            res@tiXAxisString, 0)
;            "hours since 2022-07-01 00:00:00", 0)
    print(date)
    slpc = new(mypoints, float)
    slpc!0 = "time"
    slpc&time = date 
    slpc(:) = data(:,6) * 0.01
    print(slpc)
    ;res@xyLineColor = colors(j)
    res@xyLineColor = cmap(j*color_int,:)
    ListPush(colors,(/cmap(j*color_int,:)/))
    res@xyDashPattern = 0
    ;if(j.eq.0) then
    plots(j) = gsn_csm_xy(wks, date, slpc, res)
    ;else
    ;plot2 = gsn_csm_xy(wks, date, slpc, res)
    ;overlay(plot, plot2)
    ;delete(plot2)
    ;end if
    delete(data)
    delete(nn)
    delete(ss)
    delete(date)
    delete(slpc)
    ihour = ihour + dt
  end do
  if(.not.plotbst) then
    plot = plots(nfiles-1) 
    if(nfiles.gt.1) then
      do j = 0,nfiles-2
        plot2 = plots(nfiles-2-j)
        overlay(plot, plot2)
      end do
    end if
  else
    do j = 0,nfiles-1
      plot2 = plots(nfiles-1-j)
      overlay(plot, plot2)
    end do
  end if
;; legends
  xmin = 0.13
  xwidth = 0.16
  if(nfiles.gt.4) then
  xwidth = 0.32
  end if
  xgon = (/xmin,xmin+xwidth,xmin+xwidth,xmin,xmin/)
  ygon = (/0.925,0.925,0.8,0.8,0.925/)
  xleg = (/0.150,0.150,0.150,0.150,0.300,0.300,0.300,0.300/)
  xtxt = (/0.225,0.225,0.225,0.225,0.375,0.375,0.375,0.375/)
  yleg = (/0.9,0.875,0.85,0.825,0.9,0.875,0.85,0.825/)
  ytxt = (/0.9,0.875,0.85,0.825,0.9,0.875,0.85,0.825/)
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
