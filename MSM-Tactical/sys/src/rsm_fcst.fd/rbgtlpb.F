      subroutine rbgtlpb
#include <paramodel.h>
#include <rscomspc.h>
#include <rscomltb.h>
#include <rscombgt.h>
#ifdef A
      do k=1,levr
      do i=1,lnwav
#endif
#ifdef T
        tek(i,k)=( y(i,k)-tek(i,k))*rdt2
#endif
#ifdef Q
        rqk(i,k)=(rt(i,k)-rqk(i,k))*rdt2
#endif
#ifdef U
        uuk(i,k)=( x(i,k)-uuk(i,k))*rdt2
#endif
#ifdef V
        vvk(i,k)=( w(i,k)-vvk(i,k))*rdt2
#endif
#ifdef A
      enddo
      enddo
#endif
#ifdef P
      do i=1,lnwav
      psk(i)=(z(i)-psk(i))*rdt2
      enddo
#endif
#ifdef T
      call cctogd(tek,wt(1,1,nt),levr)
      call getful(wt(1,1,nt  ),teltb,wsltb,levr)
      call getful(wt(1,1,nt+1),teltb,wsltb,levr)
#endif
#ifdef Q
      call cctogd(rqk,wq(1,1,nq),levr)
      call getful(wq(1,1,nq  ),rqltb,wsltb,levr)
      call getful(wq(1,1,nq+1),rqltb,wsltb,levr)
#endif
#ifdef U
      call sctogd(uuk,wu(1,1,nu),levr)
      call getful(wu(1,1,nu  ),uultb,wsltb,levr)
      call getful(wu(1,1,nu+1),uultb,wsltb,levr)
#endif
#ifdef V
      call cstogd(vvk,wv(1,1,nv),levr)
      call getful(wv(1,1,nv  ),vvltb,wsltb,levr)
      call getful(wv(1,1,nv+1),vvltb,wsltb,levr)
#endif
#ifdef P
      call cctogd(psk,wp(1,1,np),1)
      call getful(wp(1,1,np  ), qltb,wsltb,1)
      call getful(wp(1,1,np+1), qltb,wsltb,1)
#endif
#ifdef A
      rewind junit1
      rewind junit2
#endif
#ifdef T
      call rbgtio(junit1,junit2,wt,levr,nt+1,tmpbgt)
#endif
#ifdef Q
      call rbgtio(junit1,junit2,wq,levr,nq+1,tmpbgt)
#endif
#ifdef U
      call rbgtio(junit1,junit2,wu,levr,nu+1,tmpbgt)
#endif
#ifdef V
      call rbgtio(junit1,junit2,wv,levr,nv+1,tmpbgt)
#endif
#ifdef P
      call rbgtio(junit1,junit2,wp,    1,np+1,tmpbgt)
#endif
#ifdef A
      ktmp=junit1
      junit1=junit2
      junit2=ktmp
#endif
      return
      end
