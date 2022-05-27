t = 1
'set display color white'
'c'
'open r_pgb.ctl'
while(t<=34)
 'set t 't
 'set lev 300'
 'set mpdset hires'
 'set grads off'
 'set grid off'
 'set map 0 1 2'
 'set gxout shaded'
 'd sqrt(ugrdprs*ugrdprs+vgrdprs*vgrdprs)'
 'cbarn'
 'set gxout contour'
 'set ccolor 1'
 'd hgtprs'
 ft = t - 1
 'draw title FT='ft'H'
 'gxprint z300+wspd_ft'ft'h.png x1280 y1024 white'
 'c'
 t = t + 1
endwhile

