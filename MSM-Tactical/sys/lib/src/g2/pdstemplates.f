      module pdstemplates
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MODULE:    pdstemplates 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-11
!
! ABSTRACT: This Fortran Module contains info on all the available 
!   GRIB2 Product Definition Templates used in Section 4 (PDS).
!   Each Template has three parts: The number of entries in the template
!   (mapgridlen);  A map of the template (mapgrid), which contains the
!   number of octets in which to pack each of the template values; and
!   a logical value (needext) that indicates whether the Template needs 
!   to be extended.  In some cases the number of entries in a template 
!   can vary depending upon values specified in the "static" part of 
!   the template.  ( See Template 4.3 as an example )
!
!   This module also contains two subroutines.  Subroutine getpdstemplate
!   returns the octet map for a specified Template number, and
!   subroutine extpdstemplate will calculate the extended octet map
!   of an appropriate template given values for the "static" part of the 
!   template.  See docblocks below for the arguments and usage of these 
!   routines.
!
!   NOTE:  Array mapgrid contains the number of octets in which the 
!   corresponding template values will be stored.  A negative value in
!   mapgrid is used to indicate that the corresponding template entry can
!   contain negative values.  This information is used later when packing
!   (or unpacking) the template data values.  Negative data values in GRIB
!   are stored with the left most bit set to one, and a negative number
!   of octets value in mapgrid() indicates that this possibility should
!   be considered.  The number of octets used to store the data value
!   in this case would be the absolute value of the negative value in 
!   mapgrid().
!  
!
! PROGRAM HISTORY LOG:
! 2000-05-11  Gilbert
! 2001-12-04  Gilbert  -  Added Templates 4.12, 4.12, 4.14,
!                         4.1000, 4.1001, 4.1002, 4.1100 and 4.1101
! 2009-05-21  VUONG    -  Allow negative scale factors and limits for
!                         Templates 4.5 and 4.9
!
! USAGE:    use pdstemplates
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      integer,parameter :: MAXLEN=200,MAXTEMP=23

      type pdstemplate
          integer :: template_num
          integer :: mappdslen
          integer,dimension(MAXLEN) :: mappds
          logical :: needext
      end type pdstemplate

      type(pdstemplate),dimension(MAXTEMP) :: templates

      data templates(1)%template_num /0/     !  Fcst at Level/Layer
      data templates(1)%mappdslen /15/
      data templates(1)%needext /.false./
      data (templates(1)%mappds(j),j=1,15) 
     &                             /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4/

      data templates(2)%template_num /1/     !  Ens fcst at level/layer
      data templates(2)%mappdslen /18/
      data templates(2)%needext /.false./
      data (templates(2)%mappds(j),j=1,18)
     &                        /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1/

      data templates(3)%template_num /2/     !  Derived Ens fcst at level/layer
      data templates(3)%mappdslen /17/
      data templates(3)%needext /.false./
      data (templates(3)%mappds(j),j=1,17)
     &                      /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1/

      data templates(4)%template_num /3/     !  Ens cluster fcst rect. area
      data templates(4)%mappdslen /31/
      data templates(4)%needext /.true./
      data (templates(4)%mappds(j),j=1,31)
     &       /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,1,1,1,1,-4,-4,4,4,
     &        1,-1,4,-1,4/

      data templates(5)%template_num /4/     !  Ens cluster fcst circ. area
      data templates(5)%mappdslen /30/
      data templates(5)%needext /.true./
      data (templates(5)%mappds(j),j=1,30)
     &       /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,1,1,1,1,-4,4,4,
     &        1,-1,4,-1,4/

      data templates(6)%template_num /5/     !  Prob fcst at level/layer
      data templates(6)%mappdslen /22/
      data templates(6)%needext /.false./
      data (templates(6)%mappds(j),j=1,22)
     &          /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,-1,-4,-1,-4/

      data templates(7)%template_num /6/     !  Percentile fcst at level/layer
      data templates(7)%mappdslen /16/
      data templates(7)%needext /.false./
      data (templates(7)%mappds(j),j=1,16)
     &                     /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1/

      data templates(8)%template_num /7/     !  Error at level/layer
      data templates(8)%mappdslen /15/
      data templates(8)%needext /.false./
      data (templates(8)%mappds(j),j=1,15)
     &                     /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4/

      data templates(9)%template_num /8/     !  Ave or Accum at level/layer
      data templates(9)%mappdslen /29/
      data templates(9)%needext /.true./
      data (templates(9)%mappds(j),j=1,29)
     &  /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,2,1,1,1,1,1,1,4,1,1,1,4,1,4/

      data templates(10)%template_num /9/     !  Prob over time interval
      data templates(10)%mappdslen /36/
      data templates(10)%needext /.true./
      data (templates(10)%mappds(j),j=1,36)
     &  /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,-1,-4,-1,-4,2,1,1,1,
     &   1,1,1,4,1,1,1,4,1,4/

      data templates(11)%template_num /10/     !  Percentile over time interval
      data templates(11)%mappdslen /30/
      data templates(11)%needext /.true./
      data (templates(11)%mappds(j),j=1,30)
     &    /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,2,1,1,1,1,1,1,4,
     &     1,1,1,4,1,4/

      data templates(12)%template_num /11/     !  Ens member over time interval
      data templates(12)%mappdslen /32/
      data templates(12)%needext /.true./
      data (templates(12)%mappds(j),j=1,32)
     &    /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,2,1,1,1,1,1,1,
     &     4,1,1,1,4,1,4/

      data templates(13)%template_num /12/     !  Derived Ens fcst over time int
      data templates(13)%mappdslen /31/
      data templates(13)%needext /.true./
      data (templates(13)%mappds(j),j=1,31)
     &                   /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,
     &                    2,1,1,1,1,1,1,4,1,1,1,4,1,4/

      data templates(14)%template_num /13/     !  Ens cluster fcst rect. area
      data templates(14)%mappdslen /45/
      data templates(14)%needext /.true./
      data (templates(14)%mappds(j),j=1,45)
     &       /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,1,1,1,1,-4,-4,4,4,
     &        1,-1,4,-1,4,2,1,1,1,1,1,1,4,1,1,1,4,1,4/

      data templates(15)%template_num /14/     !  Ens cluster fcst circ. area
      data templates(15)%mappdslen /44/
      data templates(15)%needext /.true./
      data (templates(15)%mappds(j),j=1,44)
     &       /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,1,1,1,1,-4,4,4,
     &        1,-1,4,-1,4,2,1,1,1,1,1,1,4,1,1,1,4,1,4/

      data templates(16)%template_num /20/     !  Radar Product
      data templates(16)%mappdslen /19/
      data templates(16)%needext /.false./
      data (templates(16)%mappds(j),j=1,19)
     &                     /1,1,1,1,1,-4,4,2,4,2,1,1,1,1,1,2,1,3,2/

      data templates(17)%template_num /30/     !  Satellite Product
      data templates(17)%mappdslen /5/
      data templates(17)%needext /.true./
      data (templates(17)%mappds(j),j=1,5)
     &                            /1,1,1,1,1/

      data templates(18)%template_num /254/     !  CCITTIA5 Character String
      data templates(18)%mappdslen /3/
      data templates(18)%needext /.false./
      data (templates(18)%mappds(j),j=1,3)
     &                     /1,1,4/

      data templates(19)%template_num /1000/     !  Cross section
      data templates(19)%mappdslen /9/
      data templates(19)%needext /.false./
      data (templates(19)%mappds(j),j=1,9)
     &                     /1,1,1,1,1,2,1,1,4/

      data templates(20)%template_num /1001/     !  Cross section over time
      data templates(20)%mappdslen /16/
      data templates(20)%needext /.false./
      data (templates(20)%mappds(j),j=1,16)
     &                     /1,1,1,1,1,2,1,1,4,4,1,1,1,4,1,4/

      data templates(21)%template_num /1002/     !  Cross section processed time
      data templates(21)%mappdslen /15/
      data templates(21)%needext /.false./
      data (templates(21)%mappds(j),j=1,15)
     &                     /1,1,1,1,1,2,1,1,4,1,1,1,4,4,2/

      data templates(22)%template_num /1100/     !  Hovmoller grid
      data templates(22)%mappdslen /15/
      data templates(22)%needext /.false./
      data (templates(22)%mappds(j),j=1,15)
     &                     /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4/

      data templates(23)%template_num /1101/     !  Hovmoller with stat proc
      data templates(23)%mappdslen /22/
      data templates(23)%needext /.false./
      data (templates(23)%mappds(j),j=1,22)
     &               /1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,4,1,1,1,4,1,4/


      contains

         integer function getpdsindex(number)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    getpdsindex
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2001-06-28
!
! ABSTRACT: This function returns the index of specified Product
!   Definition Template 4.NN (NN=number) in array templates.
!
! PROGRAM HISTORY LOG:
! 2001-06-28  Gilbert
!
! USAGE:    index=getpdsindex(number)
!   INPUT ARGUMENT LIST:
!     number   - NN, indicating the number of the Product Definition
!                Template 4.NN that is being requested.
!
! RETURNS:  Index of PDT 4.NN in array templates, if template exists.
!           = -1, otherwise.
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: number

           getpdsindex=-1

           do j=1,MAXTEMP
              if (number.eq.templates(j)%template_num) then
                 getpdsindex=j
                 return
              endif
           enddo

         end function




         subroutine getpdstemplate(number,nummap,map,needext,iret)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    getpdstemplate 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-11
!
! ABSTRACT: This subroutine returns PDS template information for a 
!   specified Product Definition Template 4.NN.
!   The number of entries in the template is returned along with a map
!   of the number of octets occupied by each entry.  Also, a flag is
!   returned to indicate whether the template would need to be extended.
!
! PROGRAM HISTORY LOG:
! 2000-05-11  Gilbert
!
! USAGE:    CALL getpdstemplate(number,nummap,map,needext,iret)
!   INPUT ARGUMENT LIST:
!     number   - NN, indicating the number of the Product Definition 
!                Template 4.NN that is being requested.
!
!   OUTPUT ARGUMENT LIST:      
!     nummap   - Number of entries in the Template
!     map()    - An array containing the number of octets that each 
!                template entry occupies when packed up into the PDS.
!     needext  - Logical variable indicating whether the Product Defintion
!                Template has to be extended.  
!     ierr     - Error return code.
!                0 = no error
!                1 = Undefine Product Template number.
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: number
           integer,intent(out) :: nummap,map(*),iret
           logical,intent(out) :: needext

           iret=0

           index=getpdsindex(number)

           if (index.ne.-1) then
              nummap=templates(index)%mappdslen
              needext=templates(index)%needext
              map(1:nummap)=templates(index)%mappds(1:nummap)
           else
             nummap=0
             needext=.false.
             print *,'getpdstemplate: PDS Template ',number,
     &               ' not defined.'
             iret=1
           endif

         end subroutine

         subroutine extpdstemplate(number,list,nummap,map)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    extpdstemplate 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-11
!
! ABSTRACT: This subroutine generates the remaining octet map for a
!   given Product Definition Template, if required.  Some Templates can
!   vary depending on data values given in an earlier part of the 
!   Template, and it is necessary to know some of the earlier entry
!   values to generate the full octet map of the Template.
!
! PROGRAM HISTORY LOG:
! 2000-05-11  Gilbert
!
! USAGE:    CALL extpdstemplate(number,list,nummap,map)
!   INPUT ARGUMENT LIST:
!     number   - NN, indicating the number of the Product Definition 
!                Template 4.NN that is being requested.
!     list()   - The list of values for each entry in the 
!                the Product Definition Template 4.NN.
!
!   OUTPUT ARGUMENT LIST:      
!     nummap   - Number of entries in the Template
!     map()    - An array containing the number of octets that each 
!                template entry occupies when packed up into the GDS.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: number,list(*)
           integer,intent(out) :: nummap,map(*)

           index=getpdsindex(number)
           if (index.eq.-1) return

           if ( .not. templates(index)%needext ) return
           nummap=templates(index)%mappdslen
           map(1:nummap)=templates(index)%mappds(1:nummap)

           if ( number.eq.3 ) then
              N=list(27)
              do i=1,N
                map(nummap+i)=1
              enddo
              nummap=nummap+N
           elseif ( number.eq.4 ) then
              N=list(26)
              do i=1,N
                map(nummap+i)=1
              enddo
              nummap=nummap+N
           elseif ( number.eq.8 ) then
              if ( list(22).gt.1 ) then
                do j=2,list(22)
                  do k=1,6
                    map(nummap+k)=map(23+k)
                  enddo
                  nummap=nummap+6
                enddo
              endif
           elseif ( number.eq.9 ) then
              if ( list(29).gt.1 ) then
                do j=2,list(29)
                  do k=1,6
                    map(nummap+k)=map(30+k)
                  enddo
                  nummap=nummap+6
                enddo
              endif
           elseif ( number.eq.10 ) then
              if ( list(23).gt.1 ) then
                do j=2,list(23)
                  do k=1,6
                    map(nummap+k)=map(24+k)
                  enddo
                  nummap=nummap+6
                enddo
              endif
           elseif ( number.eq.11 ) then
              if ( list(25).gt.1 ) then
                do j=2,list(25)
                  do k=1,6
                    map(nummap+k)=map(26+k)
                  enddo
                  nummap=nummap+6
                enddo
              endif
           elseif ( number.eq.12 ) then
              if ( list(24).gt.1 ) then
                do j=2,list(24)
                  do k=1,6
                    map(nummap+k)=map(25+k)
                  enddo
                  nummap=nummap+6
                enddo
              endif
           elseif ( number.eq.13 ) then
              if ( list(38).gt.1 ) then
                do j=2,list(38)
                  do k=1,6
                    map(nummap+k)=map(39+k)
                  enddo
                  nummap=nummap+6
                enddo
              endif
              N=list(27)
              do i=1,N
                map(nummap+i)=1
              enddo
              nummap=nummap+N
           elseif ( number.eq.14 ) then
              if ( list(37).gt.1 ) then
                do j=2,list(37)
                  do k=1,6
                    map(nummap+k)=map(38+k)
                  enddo
                  nummap=nummap+6
                enddo
              endif
              N=list(26)
              do i=1,N
                map(nummap+i)=1
              enddo
              nummap=nummap+N
           elseif ( number.eq.30 ) then
              do j=1,list(5)
                map(nummap+1)=2
                map(nummap+2)=2
                map(nummap+3)=1
                map(nummap+4)=1
                map(nummap+5)=4
                nummap=nummap+5
              enddo
           endif

         end subroutine

         integer function getpdtlen(number)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    getpdtlen
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2004-05-11
!
! ABSTRACT: This function returns the initial length (number of entries) in 
!   the "static" part of specified Product Definition Template 4.number.
!
! PROGRAM HISTORY LOG:
! 2004-05-11  Gilbert
!
! USAGE:    CALL getpdtlen(number)
!   INPUT ARGUMENT LIST:
!     number   - NN, indicating the number of the Product Definition 
!                Template 4.NN that is being requested.
!
! RETURNS:     Number of entries in the "static" part of PDT 4.number
!              OR returns 0, if requested template is not found.
!
! REMARKS: If user needs the full length of a specific template that
!    contains additional entries based on values set in the "static" part
!    of the PDT, subroutine extpdstemplate can be used.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: number

           getpdtlen=0

           index=getpdsindex(number)

           if (index.ne.-1) then
              getpdtlen=templates(index)%mappdslen
           endif

         end function


      end module

