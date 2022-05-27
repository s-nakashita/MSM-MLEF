      common idate(4)
!
!  common for input arrays
!
      common/rinpinp/ qi(ijdimi),tei(ijdimi,kdimi),uui(ijdimi,kdimi),    &  
     &       vvi(ijdimi,kdimi),rqi(ijdimi,kdimi,ntotal),gzi(ijdimi)      &
     &      ,flati(ijdimi),floni(ijdimi)                                 &
     &      ,fm2i(ijdimi),fm2xi(ijdimi),fm2yi(ijdimi)
!
!  common for after horizontal resolution change
!
      common/rinpcur/ q (ijdimo),te (ijdimo,kdimi),uu (ijdimo,kdimi),    &
     &       vv (ijdimo,kdimi),rq (ijdimo,kdimi,ntotal),gz (ijdimo)      &
     &      ,flato(ijdimo),flono(ijdimo)                                 &
     &      ,fm2o(ijdimo),fm2xo(ijdimo),fm2yo(ijdimo)
!
!  common for arrays of modified vertical resolution
!
      common/rinpout/ qo(ijdimo),teo(ijdimo,kdimo),uuo(ijdimo,kdimo),    &  
     &       vvo(ijdimo,kdimo),rqo(ijdimo,kdimo,ntotal),gzo(ijdimo)

