      integer function ismax(idim,arr,istr)
      implicit none
      integer 	idim,istr
      real 	arr(idim)
      integer 	i
      real 	arrmax
      ismax=istr
      arrmax=arr(istr)
      do i=istr,idim
        if(arr(i).gt.arrmax) then
          arrmax=arr(i)
          ismax=i
        endif
      enddo
      return
      end

      integer function ismin(idim,arr,istr)
      implicit none
      integer 	idim,istr
      real 	arr(idim)
      integer 	i
      real 	arrmin
      ismin=istr
      arrmin=arr(istr)
      do i=istr,idim
        if(arr(i).lt.arrmin) then
          arrmin=arr(i)
          ismin=i
        endif
      enddo
      return
      end
