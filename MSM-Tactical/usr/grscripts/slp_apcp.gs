t = 1
while(t<=34)
 'set display color white'
 'c'
 'open r_pgb.ctl'
 'set t 't
 'set mpdset hires'
 'set grads off'
 'set grid off'
 'set map 12 1 2'
 if(t > 1)
  'set gxout shaded'
  'set clevs 1 5 10 20 30 50 80'
  'set ccols 0 11 13 10 7 8 2 6'
  'd apcpsfc'
  'cbarn'
 endif
 'set gxout contour'
 'set ccolor 1'
 'd prmslmsl'
 ft = t - 1
 'draw title FT='ft'H'
 'gxprint slp+apcp_ft'ft'h.png x1280 y1024 white'
 'c'
 t = t + 1
endwhile
  
