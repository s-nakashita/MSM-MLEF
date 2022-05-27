# ------------- INTEL FORTRAN ------
FORT=	ncepxlf
OPTN=	-O3

SRCS=	rsmmap.f
OBJS=	rsmmap.o
EXEC=	../../rsmmap.x

$(EXEC):	$(OBJS)
	$(FORT) -o $(EXEC) $(OPTN) $(OBJS) 
$(OBJS):	$(SRCS)
	$(FORT) -c $(OPTN) $(SRCS)
clean:
	rm $(OBJS) $(EXEC)
