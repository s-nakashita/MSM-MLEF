!-----------------------------------------------------------------------
!fpp$ noconcur r
      subroutine gtbits(ibm,ids,len,mg,g,ground,gmin,gmax,nbit)
!$$$  subprogram documentation block
!
! subprogram:    gtbits      compute number of bits and round field.
!   prgmmr: iredell          org: w/nmc23    date: 92-10-31
!
! abstract: the number of bits required to pack a given field
!   at a particular decimal scaling is computed using the field range.
!   the field is rounded off to the decimal scaling for packing.
!   the minimum and maximum rounded field values are also returned.
!   grib bitmap masking for valid data is optionally used.
!
! program history log:
!   92-10-31  iredell
!
! usage:    call gtbits(ibm,ids,len,mg,g,gmin,gmax,nbit)
!   input argument list:
!     ibm      - integer bitmap flag (=0 for no bitmap)
!     ids      - integer decimal scaling
!                (e.g. ids=3 to round field to nearest milli-value)
!     len      - integer length of the field and bitmap
!     mg       - integer (len) bitmap if ibm=1 (0 to skip, 1 to keep)
!     g        - real (len) field
!
!   output argument list:
!     ground   - real (len) field rounded to decimal scaling
!                (set to zero where bitmap is 0 if ibm=1)
!     gmin     - real minimum valid rounded field value
!     gmax     - real maximum valid rounded field value
!     nbit     - integer number of bits to pack
!
! subprograms called:
!   isrchne  - find first value in an array not equal to target value
!
! attributes:
!   language: cray fortran
!
!$$$
      dimension mg(len),g(len),ground(len)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  round field and determine extremes where bitmap is on
      ds=10.**ids
      if(ibm.eq.0) then
        ground(1)=nint(g(1)*ds)/ds
        gmax=ground(1)
        gmin=ground(1)
        do i=2,len
          ground(i)=nint(g(i)*ds)/ds
          gmax=max(gmax,ground(i))
          gmin=min(gmin,ground(i))
        enddo
      else
        i1=isrchne(len,mg,1,0)
        i1=isrchne(len,mg,1,0)
        if(i1.gt.0.and.i1.le.len) then
          do i=1,i1-1
            ground(i)=0.
          enddo
          ground(i1)=nint(g(i1)*ds)/ds
          gmax=ground(i1)
          gmin=ground(i1)
          do i=i1+1,len
            if(mg(i).ne.0) then
              ground(i)=nint(g(i)*ds)/ds
              gmax=max(gmax,ground(i))
              gmin=min(gmin,ground(i))
            else
              ground(i)=0.
            endif
          enddo
        else
          do i=1,len
            ground(i)=0.
          enddo
          gmax=0.
          gmin=0.
        endif
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute number of bits
      nbit=log((gmax-gmin)*ds+0.9)/log(2.)+1.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
