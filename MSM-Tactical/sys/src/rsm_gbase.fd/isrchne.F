c
      function isrchne(n,a,is,val)
      dimension a(n)
      isrchne=n
      do nn=is,n
      if( a(nn).ne.val ) then
        isrchne=nn
        return
      endif
      enddo
      return
      end
