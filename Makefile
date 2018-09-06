include $(ROOTSYS)/etc/Makefile.arch

SRC := $(PWD)/src
OBJDIR := $(PWD)/obj

HEADERS :=
OBJS    := $(addprefix $(OBJDIR)/,dsssdCal.o)
EXE     := dsssdCal

ifeq ($(ROOTMAJORVERSION),6)
ROOTCINT = rootcling
LINKDEF := $(SRC)/Linkdef.h
else
ROOTCINT = rootcint
LINKDEF := $(SRC)/Linkdef5.h
endif

CC  = clang
CXX = clang++

MIDASLIBS = -L$(MIDASSYS)/darwin/lib -lmidas
LIBS      = -L$(MIDASSYS)/darwin/lib -lmidas $(ROOTLIBS) -L$(DRAGONSYS)/lib -lDragon
INCLUDE   = -I$(MIDASSYS)/include -I$(SRC) -I$(DRAGONSYS)/src
CXXFLAGS  = -ggdb $(ROOTCFLAGS) $(INCLUDE) -DOS_DARWIN -DMIDASSYS -DUSE_ROOT
CINTFLAGS := $(filter-out ($(ROOTCFLAGS)), $(CXXFLAGS))

CXX  += $(CXXFLAGS) $(AUXCFLAGS) -I$(ROOTSYS)/include

LD   = $(CXX) $(LDFLAGS) $(ROOTGLIBS) $(LIBS) $(RPATH)

all: $(OBJS) dsssdCal

dsssdCal: $(EXE)

$(EXE): $(SRC)/dsssdCal.cxx $(OBJS) $(SRC)/Dict.cxx
	$(LD) $(EXPLLINKLIBS) $< -o $@
# $(CXX) $(LIBS) -I$(PWD) \
# $< $(OBJS) -lSpectrum -o $@ \


$(OBJS): | $(OBJDIR)

$(OBJDIR)/%.o: $(SRC)/%.cxx $(HEADERS) $(DRAGONSYS)/lib/DragonDictionary.cxx
	$(CXX) $(CXXFLAGS) -c -o $@ $< \

$(OBJDIR):
	mkdir $(OBJDIR)

$(SRC)/Dict.cxx: $(LINKDEF)
ifeq ($(ROOTMAJORVERSION),6)
	$(ROOTCINT) -f $@ -c $(CINTFLAGS) -p $(HEADERS) TError.h TString.h TTree.h $(LINKDEF)
else
	$(ROOTCINT) -f $@ -c $(CINTFLAGS) -p $(HEADERS) TError.h TString.h TTree.h $(LINKDEF)
endif

clean:
	 rm -f $(OBJDIR)/*.o $(EXE) $(SRC)/Dict.*

doc::
	cd doc; doxygen Doxyfile; cd ..; \
#	rsync -r doc/html gchristian@jabberwock:/Volumes/public/gchristian/public_html/dsssdCal/doc
