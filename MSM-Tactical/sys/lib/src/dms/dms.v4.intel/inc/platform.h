/*---------------------------------------------------------------------*\
 *                                                                     *
 *  Program file: platform.h                                           *
 *  Purpose: Set platform API to struct pltapi variable.               *
 *                                                                     *
\*---------------------------------------------------------------------*/

/* -------------------------------------------------------------------- */
#include "ufs_dms.h"
#include "gdb_dms.h"
#include "ipc_dms.h"
#include "net_dms.h"

int pltcnt = 1;    /* platform count */

DMS_PLT pltapi[] =
{ {"ufs",1,
   ufs_dbcrt,ufs_dbdlt,
   ufs_fcrt,ufs_fdlt,ufs_fopn,ufs_fcls,ufs_flst,ufs_frog,ufs_fchk,
   ufs_rput,ufs_rget,ufs_rdlt,ufs_rlst,
   ufs_conf,ufs_init,ufs_exit
  }
};
/* -------------------------------------------------------------------- */

