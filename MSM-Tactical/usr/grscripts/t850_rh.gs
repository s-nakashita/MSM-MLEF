t = 1
while(t<=25)
 'set display color white'
 'c'
 'open r_pgb.ctl'
 'set t 't
 'set lev 850'
 'set mpdset hires'
 'set grads off'
 'set grid off'
 'set map 0 1 2'
 'set gxout shaded'
 'd tmpprs'
 'cbarn'
 if(t > 1)
  'set gxout contour'
  'set ccolor 1'
*  'set clevs -10 -8 -6 -4 -2 2 4 6 8 10'
  'd rhprs'
 endif
 ft = t - 1
 'draw title FT='ft'H'
 'gxprint t850+rh_ft'ft'h.jpg x1280 y1024 white'
 'c'
 t = t + 1
endwhile

