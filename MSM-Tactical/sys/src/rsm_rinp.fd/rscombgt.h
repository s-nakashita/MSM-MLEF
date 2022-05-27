! ......... include budget common ........
#ifdef T
      common/rbgtt/ wt(lngrd,levr,nt+1),tek(lnwav,levr)
#endif
#ifdef Q
      common/rbgtq/ wq(lngrd,levr,nq+1),rqk(lnwav,levr)
#endif
#ifdef U
      common/rbgtu/ wu(lngrd,levr,nu+1),uuk(lnwav,levr)
#endif
#ifdef V
      common/rbgtv/ wv(lngrd,levr,nv+1),vvk(lnwav,levr)
#endif
#ifdef P
      common/rbgtp/ wp(lngrd,1,np+1),psk(lnwav)
#endif
#ifdef A
      common/rbgta/ tmpbgt(lngrd,levr),rdt2
      common/rbgti/ junit1,jnuit2,nbgt,ndst
#endif
