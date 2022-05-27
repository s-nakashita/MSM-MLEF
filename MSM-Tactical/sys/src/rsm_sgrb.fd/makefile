LIBS=	-l essl -l mass
W3LB=	-L/nwprod/lib -lbacio_4 -lw3_4
FORT=	ncepxlf
OPTx=	-Q -O3 -qxlf77=leadzero -qmaxmem=-1 -qnosave 
OPTN=	-Q -O3 -qxlf77=leadzero -qmaxmem=-1 -qnosave 
SRCS=	rsm_sgrb.f
OBJS=	rsm_sgrb.o
EXEC=	rsm_sgrb

$(EXEC):	$(OBJS)
	$(FORT) -o $(EXEC) $(OBJS) $(W3LB) $(LIBS) 
$(OBJS):	$(SRCS)
	$(FORT) -c $(OPTN) $(SRCS)
