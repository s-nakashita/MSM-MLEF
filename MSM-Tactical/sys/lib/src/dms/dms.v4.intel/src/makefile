#
# Makefile for CWB DMS system (V3.0)
#
#

#DMS_DEF = /work2/juang/CWBGSM/utl/dms/dms.v4.intel
DMS_DEF = ../

PLATFORM = LINUX
DMS_EXE = $(DMS_DEF)/$(PLATFORM)
DMS_LIB = $(DMS_EXE)/lib
GDB_LIB = $(DMS_EXE)/lib
EXE_DIR = $(DMS_EXE)/bin
INC_DIR = $(DMS_DEF)/inc

CC = icc -D$(PLATFORM) -DLX64  -I$(INC_DIR)
AR = ar q
LIBS =

DMS_OF = dms_api.o dms_int.o dms_log.o key_lib.o dms_lib.o \
        net_lib.o xdr_lib.o ufs_dms.o ipc_lib.o
UTL_OF = rdmsadt.o rdmschkr.o rdmscpy.o rdmscrt.o \
	rdmsdbcrt.o rdmsdbdlt.o rdmsdlt.o rdmsrtn.o rdmsdmp.o \
	rdmsfile.o rdmspurge.o rdmsreorg.o rdmsld.o  

LIBRDMS = $(DMS_LIB)/librdms.a
RDMSADT = $(EXE_DIR)/rdmsadt
RDMSCHKR = $(EXE_DIR)/rdmschkr
RDMSCPY = $(EXE_DIR)/rdmscpy
RDMSCRT = $(EXE_DIR)/rdmscrt
RDMSDBCRT = $(EXE_DIR)/rdmsdbcrt
RDMSDBDLT = $(EXE_DIR)/rdmsdbdlt
RDMSDLT = $(EXE_DIR)/rdmsdlt
RDMSRTN = $(EXE_DIR)/rdmsrtn
RDMSDMP = $(EXE_DIR)/rdmsdmp
RDMSFILE = $(EXE_DIR)/rdmsfile
RDMSPURGE = $(EXE_DIR)/rdmspurge
RDMSREORG = $(EXE_DIR)/rdmsreorg
RDMSLD = $(EXE_DIR)/rdmsld

default:
	@echo "\"make librdms.a\" to make the library"
	@echo "\"make all\" to make all of the DMS"
	@echo "\"make clean\" to clean all of the object file of DMS"

all:    librdms.a allrdms

librdms.a: $(LIBRDMS)
     
$(LIBRDMS) : $(DMS_OF)
	rm -f $(LIBRDMS)
	$(AR) $(LIBRDMS) $(DMS_OF) ; \
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ]; \
                then ranlib $(LIBRDMS); fi

allrdms: $(RDMSADT) $(RDMSCHKR) $(RDMSCPY) $(RDMSCRT) \
	$(RDMSDBCRT) $(RDMSDBDLT) $(RDMSDLT) $(RDMSRTN) $(RDMSDMP) \
	$(RDMSFILE) $(RDMSPURGE) $(RDMSREORG) $(RDMSLD) $(RDMSKTNS) 

$(RDMSADT): rdmsadt.o $(LIBRDMS)
	$(CC) -o $(RDMSADT) rdmsadt.o $(LIBRDMS) 

$(RDMSCHKR): rdmschkr.o $(LIBRDMS)
	$(CC) -o $(RDMSCHKR) rdmschkr.o $(LIBRDMS) 

$(RDMSCPY): rdmscpy.o $(LIBRDMS)
	$(CC) -o $(RDMSCPY) rdmscpy.o $(LIBRDMS) 

$(RDMSCRT): rdmscrt.o $(LIBRDMS)
	$(CC) -o $(RDMSCRT) rdmscrt.o $(LIBRDMS) 

$(RDMSDBCRT): rdmsdbcrt.o $(LIBRDMS)
	$(CC) -o $(RDMSDBCRT) rdmsdbcrt.o $(LIBRDMS) 

$(RDMSDBDLT): rdmsdbdlt.o $(LIBRDMS)
	$(CC) -o $(RDMSDBDLT) rdmsdbdlt.o $(LIBRDMS) 

$(RDMSDLT): rdmsdlt.o $(LIBRDMS)
	$(CC) -o $(RDMSDLT) rdmsdlt.o $(LIBRDMS) 

$(RDMSRTN): rdmsrtn.o $(LIBRDMS)
	$(CC) -o $(RDMSRTN) rdmsrtn.o $(LIBRDMS) 

$(RDMSDMP): rdmsdmp.o $(LIBRDMS)
	$(CC) -o $(RDMSDMP) rdmsdmp.o $(LIBRDMS) 

$(RDMSFILE): rdmsfile.o $(LIBRDMS)
	$(CC) -o $(RDMSFILE) rdmsfile.o $(LIBRDMS) 

$(RDMSPURGE): rdmspurge.o $(LIBRDMS)
	$(CC) -o $(RDMSPURGE) rdmspurge.o $(LIBRDMS) 

$(RDMSREORG): rdmsreorg.o $(LIBRDMS)
	$(CC) -o $(RDMSREORG) rdmsreorg.o $(LIBRDMS) 

$(RDMSLD): rdmsld.o $(LIBRDMS)
	$(CC) -o $(RDMSLD) rdmsld.o $(LIBRDMS) 

.PHONE : clean
clean:  
	rm -f $(DMS_OF) $(UTL_OF) 

# others programs
dms_api.o: $(INC_DIR)/dms.h $(INC_DIR)/dms_api.h $(INC_DIR)/platform.h \
	ufs_dms.o 
dms_int.o: $(INC_DIR)/dms_int.h
dms_log.o: $(INC_DIR)/dms_log.h
key_lib.o:
net_lib.o:
dms_lib.o:
xdr_lib.o:
ipc_lib.o:
ufs_dms.o:

# rdms programs
rdmsadt.o: dms_api.o
rdmschkr.o: dms_api.o
rdmscpy.o: dms_api.o
rdmscrt.o: dms_api.o
rdmsdbcrt.o: dms_api.o
rdmsdbdlt.o: dms_api.o
rdmsdlt.o: dms_api.o
rdmsrtn.o: dms_api.o
rdmsdmp.o: dms_api.o
rdmsfile.o: dms_api.o
rdmspurge.o: dms_api.o
rdmsreorg.o: dms_api.o
rdmsld.o: dms_api.o
