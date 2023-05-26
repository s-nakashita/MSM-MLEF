      subroutine rsetbgt(dt2,limlow,maxstp,nrbgt,nrbgt1,nrbgt2)
#include <paramodel.h>
#include <rscombgt.h>
#ifdef A
      rdt2=1./dt2
      ndst=maxstp-limlow+1
      nbgt=nrbgt
      junit1=nrbgt1
      junit2=nrbgt2
#endif
#ifdef T
      call rbgtini(junit1,tmpbgt,levr,nt+1)
#endif
#ifdef Q
      call rbgtini(junit1,tmpbgt,levr,nq+1)
#endif
#ifdef U
      call rbgtini(junit1,tmpbgt,levr,nu+1)
#endif
#ifdef V
      call rbgtini(junit1,tmpbgt,levr,nv+1)
#endif
#ifdef P
      call rbgtini(junit1,tmpbgt,    1,np+1)
#endif
#ifdef A
      return
#endif
      end
