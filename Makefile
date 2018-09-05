include /Users/dragon/packages/root/v5-34-05/etc/Makefile.arch

# CC  = clang
# CXX = clang++

MIDASLIBS = -L$(MIDASSYS)/darwin/lib -lmidas
LIBS      = -L$(MIDASSYS)/darwin/lib -lmidas $(ROOTLIBS) -L$(DRAGONSYS)/lib -lDragon
INCLUDE   = -I$(MIDASSYS)/include -I$(SRC) -I$(DRAGONSYS)/src
CXXFLAGS  = -ggdb $(ROOTCFLAGS) $(INCLUDE) -DOS_DARWIN -DMIDASSYS -DUSE_ROOT
CINTFLAGS := $(filter-out ($(ROOTCFLAGS)), $(CXXFLAGS))


CXX  += $(CXXFLAGS) $(AUXCFLAGS) -I$(ROOTSYS)/include

LD   = $(CXX) $(LDFLAGS) $(ROOTGLIBS) $(LIBS) $(RPATH)

SRC := $(PWD)/src
OBJDIR := $(PWD)/obj

HEADERS  :=
OBJS  := $(addprefix $(OBJDIR)/,dsssdCal.o)

all: $(OBJS) dsssdCal

dsssdCal: $(SRC)/dsssdCal.cxx $(OBJS) $(SRC)/Dict.cxx
	$(LD) $(EXPLLINKLIBS) $< -o $@
# $(CXX) $(LIBS) -I$(PWD) \
# $< $(OBJS) -lSpectrum -o $@ \


$(OBJS): | $(OBJDIR)

$(OBJDIR)/%.o: $(SRC)/%.cxx $(HEADERS) $(DRAGONSYS)/lib/DragonDictionary.cxx
	$(CXX) $(CXXFLAGS) -c -o $@ $< \

$(OBJDIR):
	mkdir $(OBJDIR)

$(SRC)/Dict.cxx: $(SRC)/Linkdef.h
	rootcint -f $@ -c $(CINTFLAGS) -p $(HEADERS) $(SRC)/Linkdef.h \

clean:
	 rm -f $(OBJDIR)/*.o dsssdCal $(SRC)/Dict.*

doc::
	cd doc; doxygen Doxyfile; cd ..; \
#	rsync -r doc/html gchristian@jabberwock:/Volumes/public/gchristian/public_html/dsssdCal/doc
