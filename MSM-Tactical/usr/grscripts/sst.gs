* sfc.fNN.bin and sfc.fNN.ctl are created by post/post_sfc.sh
function sst(fh)
'set display color white'
'c'
'set mpdset hires'
'open sfc.f'fh'.ctl'
'set grads off'
'set gxout shaded'
'd maskout(tsea, -slmsk)'
'cbarn'
* draw date
'q dims'
temp=sublin(result,5)
tim=subwrd(temp,6)
'draw title 'tim
'gxprint sst_f'fh'.png x1280 y1024 white'
return
