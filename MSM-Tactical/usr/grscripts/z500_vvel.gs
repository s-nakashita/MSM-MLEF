t = 1
while(t<=34)
 'set display color white'
 'c'
 'open r_pgb.ctl'
 'set t 't
 'set lev 500'
 'set mpdset hires'
 'set grads off'
 'set grid off'
 'set map 12 1 2'
 if(t > 1)
  'set gxout shaded'
  'set clevs -10 -8 -6 -4 -2 2 4 6 8 10'
  'set ccols 9 4 11 5 3 0 7 12 8 2 6'
  'd vvelprs'
  'cbarn'
 endif
 'set gxout contour'
 'set ccolor 1'
 'd hgtprs'
 ft = t - 1
 'draw title FT='ft'H'
 'gxprint z500+vvel_ft'ft'h.png x1280 y1024 white'
 'c'
 t = t + 1
endwhile

