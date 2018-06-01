PFUNIT = /usr/local
F90_VENDOR = Intel
F90 = gfortran
include $(PFUNIT)/include/base.mk

FFLAGS +=  -pedantic -I$(PFUNIT)/mod
LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

PFS = $(wildcard *.pf)
OBJS = $(PFS:.pf=.o)

%.f90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@

%.o: %.f90
	$(F90) $(FFLAGS) -c $<

test: testSuites.inc mult.o $(OBJS)
	$(F90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -I. \
		$(PFUNIT)/include/driver.F90 \
./*$(OBJ_EXT) $(LIBS) $(FFLAGS)
