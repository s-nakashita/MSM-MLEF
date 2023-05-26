c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    pdsens.f    packs grib pds extension 41- for ensemble
c   prgmmr: zoltan toth      org: w/nmc20    date: 95-03-14
c
c abstract: packs brib pds extension starting on byte 41 for ensemble
c forecast products. for format of pds extension, see nmc office note 38
c
c program history log:
c   95-03-14  zoltan toth and mark iredell
c
c usage:    call pdsens.f(kens,kprob,xprob,kclust,kmembr,ilast,msga)
c   input argument list:
c     kens(5)  - bytes 41-45 (general section, always present.)
c     kprob(2) - bytes 46-47 (probability section, present only if neede
c     xprob(2) - bytes 48-51&52-55 (probability section, if needed.)
c     kclust(16)-bytes 61-76 (clustering section, if needed.)
c     kmembr(80)-bytes 77-86 (cluster membership section, if needed.)
c     ilast    - last byte to be packed (if greater or equal to first by
c                in any of four sections above, whole section is packed.
c
c   output argument list:      (including work arrays)
c     msga     - full pds section, including new ensemble extension
c
c remarks: use pdseup.f for unpacking pds ensemble extension.
c
c attributes:
c   language: cf77 fortran
c   machine:  cray, workstations
c
c$$$
c testing grib extension 41- packer and unpacker subroutines
c
cfpp$ noconcur r
      subroutine pdsens(kens,kprob,xprob,kclust,kmembr,ilast,msga)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    pdsens.f    packs grib pds extension 41- for ensemble
c   prgmmr: zoltan toth      org: w/nmc20    date: 95-03-14
c
c abstract: packs brib pds extension starting on byte 41 for ensemble
c       forecast products. for format of pds extension, see nmc office note 38
c
c program history log:
c   95-03-14  zoltan toth and mark iredell
c
c usage:    call pdsens.f(kens,kprob,xprob,kclust,kmembr,ilast,msga)
c   input argument list:
c     kens(5)  - bytes 41-45 (general section, always present.)
c     kprob(2) - bytes 46-47 (probability section, present only if neede
c     xprob(2) - bytes 48-51&52-55 (probability section, if needed.)
c     kclust(16)-bytes 61-76 (clustering section, if needed.)
c     kmembr(80)-bytes 77-86 (cluster membership section, if needed.)
c     ilast    - last byte to be packed (if greater or equal to first by
c                in any of four sections above, whole section is packed.
c
c   output argument list:      (including work arrays)
c     msga     - full pds section, including new ensemble extension
c
c remarks: use pdseup.f for unpacking pds ensemble extension.
c
c attributes:
c   language: cf77 fortran
c   machine:  cray, workstations
c
c$$$
c       testing grib extension 41- packer and unpacker subroutines
c
      integer kens(5),kprob(2),kclust(16),kmembr(80)
      dimension xprob(2)
      character*1 msga(100)
      if(ilast.lt.41) then
      print*,'warning - subroutine is for packing bytes 41 and above'
          go to 333
          endif
c       packing is done in four sections ending at byte il
          if(ilast.ge.41) il=45
          if(ilast.ge.46) il=55
          if(ilast.ge.61) il=76
          if(ilast.ge.77) il=86
c       changing the number of bytes (first three bytes in pds)
          call sbyte(msga, il, 0,24)
c       packing first section (general intormation section)
      if(il.ge.45) call sbytes(msga,kens,40*8,8,0,5)
c       packing 2nd section (probability section)
      if(il.ge.55) then
          call sbytes(msga,kprob,45*8,8,0,2)
          call w3fi01(lw)
          call w3fi76(xprob(1),iexp,imant,8*lw)
          call sbyte(msga,iexp,47*8,8)
          call sbyte(msga,imant,48*8,24)
          call w3fi76(xprob(2),iexp,imant,8*lw)
          call sbyte(msga,iexp,51*8,8)
          call sbyte(msga,imant,52*8,24)
      endif
c       packing 3rd section (clustering information)
      if(il.ge.76) call sbytes(msga,kclust,60*8,8,0,16)
c       packing 4th section (cluster membership)
      if(il.ge.86) call sbytes(msga,kmembr,76*8,1,0,10)
c
333   continue
          return
          end
