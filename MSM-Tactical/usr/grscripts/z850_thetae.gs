*'rc = gsfallow("on")'
'set display color white'
'c'
'open r_pgb.ctl'
t = 1
*'theta(tmpprs,rhprs)'
while(t<=25)
 'set t 't
 'set lev 850'
 'set mpdset hires'
 'set grads off'
 'set grid off'
 'set map 0 1 2'
*function theta(tmp,moist,surf,kind)
'tmp='tmpprs
'moist='rhprs
*if(kind='kind' | kind='-rh');k=1;endif
*if(kind='-r');k=2;endif
*if(kind='-e');k=3;endif
*if(kind='-q');k=4;endif
*if(surf='surf' | surf='lev' | surf='-');'press=lev';else
*  'press='surf'/100'
*endif
'press=lev'
'theta=tmp*pow((1000/press),287/1004)'
*if(k=1);* RELATIVE HUMIDITY
  'es=6.1173*exp(((2.501*pow(10,6))/461.50)*(1/273.16 - 1/tmp))'
  'ws=621.97*(es/(lev-es))'
  'w=(moist*ws)/(100*1000)'
  'thetae=(tmp+((2.501*pow(10,6))/1004)*w)*pow((1000/press),287/1004)'
*endif
*if(k=2);* Mixing Ratio
*  'thetae=(tmp+((2.501*pow(10,6))/1004)*moist)*pow((1000/press),287/1004)'
*endif
*if(k=3);* Vapor Pressure
*  'w=0.622*(moist/(lev-moist))
*  'thetae=(tmp+((2.501*pow(10,6))/1004)*w)*pow((1000/press),287/1004)'
*endif
*if(k=4);* Specific Humidity
*  'w=moist/(1-moist)'
*  'thetae=(tmp+((2.501*pow(10,6))/1004)*w)*pow((1000/press),287/1004)'
*endif
say '----------------------------'
say 'Potential temperature is defined as: theta'
say 'Equivalent potential tempearture is defined as: thetae'
say '----------------------------'
*return
 'set gxout shaded'
 'd thetae'
 'cbarn'
 'set gxout contour'
 'set ccolor 1'
 'd hgtprs'
 ft = t - 1
 'draw title FT='ft'H'
 'gxprint z850+thetae_ft'ft'h.png x1280 y1024 white'
 'c'
 t = t + 1
endwhile

