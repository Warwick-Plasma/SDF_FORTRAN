# Choose compiler (intel, pgi, g95 or gfortran) using FLAVOUR= when
# invoking make or get the default of pgi
FLAVOUR ?= pgi

# Architecture... (default 64 bit, use ARCH=linux-ix86 on command line
# for 32 bit)
ARCH ?= linux-ix86_64

SRCDIR = src
OBJDIR = obj
LIBDIR = lib
INCDIR = include
GEN_SRC = $(SRCDIR)/pack.sh

CPU = p7

ifeq (linux-ix86_64,$(ARCH))
   M64_FLAG = -m64
else
   M64_FLAG =
endif

# Compiler definitions (assumes MPI compiler on search path)
MY_FC = mpif90
MY_CC = mpicc
MODULEFLAG = -module $(INCDIR)
MY_FFLAGS = -I$(INCDIR)

# Support for multiple compiler flags

ifeq (g95,$(findstring g95,$(FLAVOUR)))
   MY_FFLAGS += -Wall -Wimplicit-none -fPIC -Wno=155 $(M64_FLAG)
   MY_FFLAGS_OPT = $(MY_FFLAGS) -O3
   MY_FFLAGS_DBG = $(MY_FFLAGS) -O0 -g -ftrace=full -fbounds-check -fzero \
       -fimplicit-none -fbounds-check
   MY_FFLAGS_PROF = $(MY_FFLAGS_OPT) -p
   MODULEFLAG = -fmod=$(OBJDIR)
endif

ifeq (gfortran,$(findstring gfortran,$(FLAVOUR)))
   MY_FFLAGS += -Wall -frecord-marker=4
   MY_FFLAGS_OPT = $(MY_FFLAGS) -O3
   MY_FFLAGS_DBG = $(MY_FFLAGS) -O0 -g -fimplicit-none -fbounds-check \
       -fbacktrace -Wextra -ffpe-trap=invalid,zero,overflow -pedantic 
   MY_FFLAGS_PROF = $(MY_FFLAGS_OPT) -p
   MODULEFLAG = -I/usr/include -I$(INCDIR) -J$(INCDIR)
   INFO_FLAGS = -Wno-conversion -fno-range-check
endif

ifeq (intel,$(findstring intel,$(FLAVOUR)))
   MY_FFLAGS += -fpe0 -mcmodel=medium -heap-arrays 64
   MY_FFLAGS_OPT = $(MY_FFLAGS) -O3 -ip -vec-report0
   MY_FFLAGS_DBG = $(MY_FFLAGS) -O0 -g -u -ftrapuv -traceback -nothreads \
       -fltconsistency -C -warn -save-temps -fpic
   MY_FFLAGS_PROF = $(MY_FFLAGS_OPT) -p
endif

ifeq (pgi,$(findstring pgi,$(FLAVOUR)))
   MY_FFLAGS += -Mnodefaultunit -Ktrap=fp -Mdclchk -tp $(CPU) -mcmodel=medium
   MY_FFLAGS_OPT = $(MY_FFLAGS) -O2 -Mvect -Munroll
   MY_FFLAGS_DBG = $(MY_FFLAGS) -O0 -g -Ktrap=denorm -Mbounds -Mchkfpstk \
       -Mdepchk -Mstandard -C
   MY_FFLAGS_PROF = $(MY_FFLAGS_OPT) -Mprof=func,lines
endif



# utils
ECHO    = echo
RM      = rm
MKDIR   = mkdir

# compiler & archiver
FC  = $(MY_FC)
AR  = ar
RANLIB = ranlib

# default mode (max. optimization)
mode = opt

FFLAGS =

# add flags for debugging if requested
ifeq (dbg,$(findstring dbg,$(mode)))
   FFLAGS  += $(MY_FFLAGS_DBG)
endif

# add flags for profiling if requested
ifeq (pro,$(findstring pro,$(mode)))
   FFLAGS  += $(MY_FFLAGS_PROF)
endif

# add flags for optimization if requested
ifeq (opt,$(findstring opt,$(mode)))
   FFLAGS  += $(MY_FFLAGS_OPT)
endif

FC_INFO := $(shell ${FC} --version | grep '[a-zA-Z]' | head -n 1)

# objectlist file
include Makefile-objs

# target name
LIB = $(LIBDIR)/libsdf.a

VPATH = $(SRCDIR):$(OBJDIR):$(LIBDIR):$(INCDIR)

# target
all: $(LIB)

# Not real file targets
.PHONY: Makefile Makefile-deps Makefile-objs all clean cleanall help

.SUFFIXES: .o .f90

# implicit rules
%.o: %.f90
	$(FC) -c $(FFLAGS) $(MODULEFLAG) -o $(OBJDIR)/$@ $<

$(SRCDIR)/sdf_source_info.f90: $(SOURCE_ALL)
	$(GEN_SRC) $@ "$(FC_INFO)" "$(FFLAGS)" $^
sdf_source_info.o: sdf_source_info.f90 $(SOURCE_ALL)
	$(FC) -c $(FFLAGS) $(INFO_FLAGS) $(MODULEFLAG) -o $(OBJDIR)/$@ $<

$(LIB): $(OBJS)
	$(RM) -f $@
	$(AR) -rsu $@ $(addprefix $(OBJDIR)/,$(OBJS))
	$(RANLIB) $@

$(OBJS): | $(OBJDIR) $(INCDIR)

$(OBJDIR):
	$(MKDIR) -p $(OBJDIR)

$(INCDIR):
	$(MKDIR) -p $(INCDIR)

$(LIB): | $(LIBDIR)

$(LIBDIR):
	$(MKDIR) -p $(LIBDIR)

# cleanup
clean:
	$(RM) -rf $(OBJDIR)

cleanall:
	$(RM) -rf $(OBJDIR) $(INCDIR) $(LIBDIR)

# help page
help:
	@$(ECHO) "Defined targets:"
	@$(ECHO) "  all    : build targets (default)"
	@$(ECHO) "  clean  : cleanup"
	@$(ECHO) "Defined modes:"
	@$(ECHO) "  opt: enable flags for optimization (default)"
	@$(ECHO) "  dbg: enable flags for debugging"
	@$(ECHO) "  pro: enable flags for profiling"
	@$(ECHO) "Example:"
	@$(ECHO) "  type \`make mode=dbg+pro' to enable dbg and pro flags"

# dependencies file
include Makefile-deps
