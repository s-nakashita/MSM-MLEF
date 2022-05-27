program prtkgb
  implicit none
  character*255 cg1
  integer iargc,lg1,lcg1,iret,j,kg,kf,k,ib
  integer jpds(200),jgds(200),jens(200)
  integer kpds(200),kgds(200),kens(200)
  if(iargc().ne.1) then
    call eusage
    call errexit(1)
  endif
  lg1=11
  call getarg(1,cg1)
  lcg1=len_trim(cg1)
  call baopenr(lg1,cg1(1:lcg1),iret)
  if(iret.ne.0) then
    call errmsg('prtkgb:  error accessing file '//cg1(1:lcg1))
    call errexit(8)
  endif
  j=0
  jpds=-1
  jgds=-1
  jens=-1
  do
    kpds=-huge(kpds)
    kgds=-huge(kgds)
    kens=-huge(kens)
    call getgbeh(lg1,0,j,jpds,jgds,jens,kg,kf,k,kpds,kgds,kens,iret)
    if(iret.ne.0) exit
    print '("Record ",i6,2x," Length ",i12," bytes ",i12," points")',k,kg,kf
    print '(("Record ",i6,2x," KPDS(",i3.3,":",i3.3,")=",5(1x,i9)))',&
     (k,ib+1,ib+5,kpds(ib+1:ib+5),ib=0,200-1,5)
    print '(("Record ",i6,2x," KGDS(",i3.3,":",i3.3,")=",5(1x,i9)))',&
     (k,ib+1,ib+5,kgds(ib+1:ib+5),ib=0,200-1,5)
    print '(("Record ",i6,2x," KENS(",i3.3,":",i3.3,")=",5(1x,i9)))',&
     (k,ib+1,ib+5,kens(ib+1:ib+5),ib=0,200-1,5)
    j=j+1
  enddo
  print '("Total: ",i6," GRIB messages in file ",a)',j,cg1(1:lcg1)
contains
  subroutine eusage
    call errmsg('usage: prtkgb gribfile')
  end subroutine
end program
