      subroutine rbgt000
#include <paramodel.h>
#include <rscombgt.h>
#ifdef T
      call rbgtzero(wt,levr,nt+1)
#endif
#ifdef Q
      call rbgtzero(wq,levr,nq+1)
#endif
#ifdef U
      call rbgtzero(wu,levr,nu+1)
#endif
#ifdef V
      call rbgtzero(wv,levr,nv+1)
#endif
#ifdef P
      call rbgtzero(wp,    1,np+1)
#endif
#ifdef A
      return
#endif
      end
