LIBS=	-l essl -l mass
W3Lx=	/nwprod/w3lib90/bacio_4_pwr3 /nwprod/w3lib90/w3lib_4_pwr3
W3LB=	/nwprod/w3lib90/bacio_4_604 /nwprod/w3lib90/w3lib_4_604
FORT=	xlf
OPTx=	-Q -O3 -qarch=pwr3 -qxlf77=leadzero -qmaxmem=-1 -qnosave 
OPTN=	-Q -O3 -qarch=604 -qxlf77=leadzero -qmaxmem=-1 -qnosave 
SRCS=	rsm_sgrb.f
OBJS=	rsm_sgrb.o
EXEC=	rsm_sgrb

$(EXEC):	$(OBJS)
	$(FORT) -o $(EXEC) $(OBJS) $(W3LB) $(LIBS) 
$(OBJS):	$(SRCS)
	$(FORT) -c $(OPTN) $(SRCS)
