#undef MP
#undef SINGLE
#undef THREAD
#undef HYBRID

#undef RKN

#undef RAS

#undef CNT
#undef CNT_1
#undef CNT_2
#undef CNT_3
#undef CNT_4
#undef CNT_5
#undef CNT_6

#undef G2R
#undef C2R

#undef CRA
#undef ORIGIN
#undef DEC
#undef IBMSP

#undef FFD
#undef FFS
#undef FF
#undef FTC
#undef FT9
#undef FT

#undef CRAY_THREAD
#undef ORIGIN_THREAD
#undef OPENMP

#undef FFT99M
#undef DCRFT
#undef INTDTB
#undef SLRFIX

#undef A
#undef T
#undef Q
#undef U
#undef V
#undef P

#undef SQK
#undef SPT

#include <defopt.h>

#ifdef THREAD
#define OPENMP
#endif
#ifdef HYBRID
#define OPENMP
#endif
#ifdef DCRFT
#define FFD
#define FF
#endif
#ifdef FFT99M
#define FT9
#define FT
#endif
#ifdef RFFTMLT
#define FTC
#define FT
#endif

#ifdef CRA
#ifdef THREAD
#define CRAY_THREAD
#endif
#ifdef HYBRID
#define CRAY_THREAD
#endif
#define ASSIGN
#define FT
#ifdef DCRFT
#define FTC
#else
#define FT9
#endif
#endif

#ifdef ORIGIN
#ifdef THREAD
#define ORIGIN_THREAD
#endif
#ifdef HYBRID
#define ORIGIN_THREAD
#endif
#endif

#ifdef DEC
#ifdef THREAD
#define OPENMP
#endif
#ifdef HYBRID
#define OPENMP
#endif
#endif
