      subroutine ctlvar(nn,numvar,numsfc)
      character varsfc*8,varwrt*8
      character var*5,sfc*3,dsl*50
      common /comctl/ var(255),sfc(255),dsl(255)
      save numvars,numsfcs,nums,nns
      data numvars,numsfcs,nums,nns/4*0/
!
      if( numvar.ne.numvars .or. numsfc.ne.numsfcs ) then
        if(numvars.ne.0) then
          write(varsfc,'(a5,a3)') var(numvars),sfc(numsfcs)
          varwrt='        '
          m=0
          do n=1,8
            if(varsfc(n:n).ne.'-') then
              m=m+1
              varwrt(m:m)=varsfc(n:n)
            endif
          enddo
          if(nums.eq.1) nums=0
          write(nns,200) varwrt,                                        &
     &                nums,numvars,numsfcs,dsl(numvars)
200       format(a8,i8,i4,',',i3,',0 ',a50)
        endif
        nums=1
      else
        nums=nums+1
      endif
!
      nns=nn
      numvars=numvar
      numsfcs=numsfc
!
      return
      end
