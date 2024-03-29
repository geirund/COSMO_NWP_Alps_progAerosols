# Makefile for compiling COSMO
# fuo, 07.03.2008, oliver.fuhrer@meteoswiss.ch
#
# Description: This makefile automagically searches for source files in the src
#              directory and creates dependencies. Afterwards files are compiled
#              and linked together.
#              Suitable for both, cosmo and int2lm
# History:
#   fuo  07.03.2008   first release


### Macros ###########################

# define shell
SHELL      := /bin/sh -l 

# define the name of the target
TARGET     := cosmo_inp_ccn_prog
 
# directory definitions
SRCDIR     := src
OBJDIR     := obj
ifeq (0,${MAKELEVEL})
  ROOT      := $(shell pwd)
  VPATH     := .:$(ROOT)/$(SRCDIR)
endif

# generate list of object files
-include $(ROOT)/Objfiles_pgi
ifndef OBJ
  $(error Could not load Objfiles file)
endif

# dynamically generated dependency file
DEPF       := .depend
IGN        := --ignore iso_fortran_env --ignore iso_c_binding
IGN        += --ignore messy_main_data_bi --ignore messy_main_timer --ignore messy_main_mpi_bi
IGN        += --ignore messy_main_blather_bi --ignore messy_main_channel_bi
IGN        += --ignore messy_main_constants_mem --ignore messy_main_timer_bi
IGN        += --ignore messy_main_tools --ignore messy_main_tracer
IGN        += --ignore messy_main_tracer_bi --ignore messy_main_tracer_mem_bi
IGN        += --ignore mmd_handle_communicator
IGN        += --ignore mo_fortran_units
IGN        += --ignore mo_mpi
IGN        += --ignore mo_rad
IGN        += --ignore oas_cos_vardef
IGN        += --ignore src_conv_ifs
#Do not ignore for TWOMOM_SB=1 compilation
#IGN        += --ingore art_aci
#IGN        += --ignore src_twomom_sb_interface
#IGN        += --ignore wolken_konstanten
#IGN        += --ignore src_cloud_opt_reff
# select machine dependent stuff
-include $(ROOT)/Options.daint.pgi
ifndef F90
  $(error Must create platform specific Options file)
endif

# add some default defines
FFLAGS += -DHAS_IOMSG

# setup flags
FFLAGS0 = $(FFLAGS)
FFLAGS1 = $(FFLAGS)
FFLAGS2 = $(FFLAGS)

# handle options
ifdef VERBOSE
  FFLAGS0  += $(VFLAGS)
  FFLAGS1  += $(VFLAGS)
  FFLAGS2  += $(VFLAGS)
endif
ifdef DEBUG
  FFLAGS0  += $(FDBG)
  FFLAGS1  += $(FDBG)
  FFLAGS2  += $(FDBG)
  LIB      += $(DBGL)
  INC      += $(DBGI)
endif
ifdef OPT
  FFLAGS0  += $(FOPT0)
  FFLAGS1  += $(FOPT1)
  FFLAGS2  += $(FOPT2)
  LIB      += $(OPTL)
  INC      += $(OPTI)
endif
ifdef MPI
  LIB      += $(MPIL)
  INC      += $(MPII)
  IGN      += --ignore mpi
else
  PFLAGS   += -DNOMPI
  OBJ      += dummy_mpi.o
endif
ifdef NUDGING
  PFLAGS   += -DNUDGING
endif
ifdef POLLEN
  PFLAGS   += -DPOLLEN
else
  IGN      += --ignore data_pollen --ignore organize_pollen
  IGN      += --ignore pol_emissions
endif
ifdef COSMOART
  PFLAGS   += -DCOSMOART
else
  IGN      += --ignore data_cosmo_art --ignore art_papa
  IGN      += --ignore art_aerosol_const
  IGN      += --ignore art_species_data
endif
ifdef PGI_FORTRAN
  PFLAGS   += -D__PGI_FORTRAN__
endif
ifdef RTTOV7
  PFLAGS   += -DRTTOV7
  LIB      += $(RTTOV7L)
  INC      += $(RTTOV7I)
endif
ifdef RTTOV10
  PFLAGS   += -DRTTOV10
  LIB      += $(RTTOV10L)
  INC      += $(RTTOV10I)
else
  IGN      += --ignore mo_rttov_ifc
endif
ifdef GRIBDWD
  PFLAGS   += -DGRIBDWD
  LIB      += $(GRIBDWDL)
  INC      += $(GRIBDWDI)
endif
ifdef GRIBAPI
  PFLAGS   += -DGRIBAPI
  LIB      += $(GRIBAPIL)
  INC      += $(GRIBAPII)
  IGN      += --ignore grib_api
else
  IGN      += --ignore grib_api
endif
ifdef NETCDF
  PFLAGS   += -DNETCDF
  LIB      += $(NETCDFL)
  INC      += $(NETCDFI)
  IGN      += --ignore netcdf
else
  IGN      += --ignore netcdf
endif
ifdef TWOMOM_SB
  PFLAGS   += -DTWOMOM_SB -DFOR_LM
  OBJ      += src_twomom_sb.o src_twomom_sb_interface.o
endif
export ROOT VPATH MACH VERBOSE DEBUG OPT MPI NUDGING POLLEN COSMOART RTTOV7 RTTOV10 GRIBDWD GRIBAPI NETCDF TWOMOM_SB PGI_FORTRAN
export CRAYPE_LINK_TYPE := dynamic
### Phony targets ###########################

.PHONY : default depend paropt pardebug seqopt seqdebug clean info

default : paropt

depend :
	@echo "generating dependencies"
	@$(ROOT)/bin/sfmakedepend --case down --longpath $(INC) $(IGN) --file $(ROOT)/$(DEPF) $(ROOT)/$(SRCDIR)/*.f90

info :
	@echo "generating compile information"
	@-rm -f $(ROOT)/.fconfig
	@echo "Target           : $(TARGET)" > $(ROOT)/.fconfig
	@echo "Compiler command : $(F90)" > $(ROOT)/.fconfig
	@echo "Compiler version : "`$(F90) -V 2>/dev/null | grep pgf` >> $(ROOT)/.fconfig
	@echo "Compiler includes: $(INC)" >> $(ROOT)/.fconfig
	@echo "Compiler flags   : $(PFLAGS) $(FFLAGS1)" >> $(ROOT)/.fconfig
	@echo "Linker command   : $(LD)" >> $(ROOT)/.fconfig
	@echo "Linker version   : "`$(LD) -V 2>/dev/null | grep pgf` >> $(ROOT)/.fconfig
	@echo "Linker flags     : $(LFLAGS) $(FFLAGS1)" >> $(ROOT)/.fconfig
	@echo "Linker libraries : $(LIB)" >> $(ROOT)/.fconfig
	@$(ROOT)/bin/gen_info.sh $(ROOT)/.fconfig $(ROOT)/$(SRCDIR)
	#@-rm -f $(ROOT)/.fconfig

paropt :
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile PGI_FORTRAN=1 NUDGING=1 TWOMOM_SB=1 RTTOV7=1 GRIBDWD=1  NETCDF=1 MPI=1 OPT=1 depend info
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile PGI_FORTRAN=1 NUDGING=1 TWOMOM_SB=1 RTTOV7=1 GRIBDWD=1  NETCDF=1 MPI=1 OPT=1 $(ROOT)/$(TARGET)

pardebug :
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile PGI_FORTRAN=1 NUDGING=1 TWOMOM_SB=1 RTTOV7=1 GRIBDWD=1  NETCDF=1 MPI=1 DEBUG=1 depend info
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile PGI_FORTRAN=1 NUDGING=1 TWOMOM_SB=1 RTTOV7=1 GRIBDWD=1  NETCDF=1 MPI=1 DEBUG=1 $(ROOT)/$(TARGET)

seqopt :
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile PGI_FORTRAN=1 NUDGING=1 TWOMOM_SB=1 RTTOV7=1 GRIBDWD=1  NETCDF=1 OPT=1 depend info
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile PGI_FORTRAN=1 NUDGING=1 TWOMOM_SB=1 RTTOV7=1 GRIBDWD=1  NETCDF=1 OPT=1 $(ROOT)/$(TARGET)

seqdebug :
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile PGI_FORTRAN=1 NUDGING=1 TWOMOM_SB=1 RTTOV7=1 GRIBDWD=1  NETCDF=1 DEBUG=1 depend info
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile PGI_FORTRAN=1 NUDGING=1 TWOMOM_SB=1 RTTOV7=1 GRIBDWD=1  NETCDF=1 DEBUG=1 $(ROOT)/$(TARGET)

$(TARGET) :
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile depend info
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile $(ROOT)/$(TARGET)

clean :
	-rm -f $(DEPF) $(DEPF).old $(OBJDIR)/*

$(ROOT)/$(TARGET) : $(OBJ)
	@echo "linking $@"
	@$(LD) $(LFLAGS) $(FFLAGS1) $(INC) $(OBJ) $(LIB) -o $@

### Suffix Rules ###########################

.SUFFIXES: .o .mod .f90

# standard suffix rules (with optimization)
%.o : %.f90
	@echo "compiling $(patsubst %.o,%.f90,$(notdir $@))"
	@$(F90) -c $(PFLAGS) $(FFLAGS1) $(INC) -o $@ $(ROOT)/$(SRCDIR)/$(patsubst %.o,%.f90,$(notdir $@))
%.o : %.f
	@echo "compiling $(patsubst %.o,%.f,$(notdir $@))"
	@$(F90) -c $(PFLAGS) $(FFLAGS1) $(INC) -Mnofree -Mnodclchk -o $@ $(ROOT)/$(SRCDIR)/$(patsubst %.o,%.f,$(notdir $@))

# NO OPTIMIZATION
	-module unload perftools-cscs/645-cuda
src_setup_vartab.o : $(ROOT)/$(SRCDIR)/src_setup_vartab.f90
	@echo "compiling $(patsubst %.o,%.f90,$(notdir $@))"
	@$(F90) -c $(PFLAGS) $(FFLAGS0) $(INC) -o $@ $(ROOT)/$(SRCDIR)/$(patsubst %.o,%.f90,$(notdir $@))

	-module unload perftools-cscs/645-cuda
# special suffix rule (for files without optimization)
$(NOOPT): %.o : %.f90
	@echo "compiling $(patsubst %.o,%.f90,$(notdir $@)) (without optimization)"
	@$(F90) -c $(PFLAGS) $(FFLAGS0) $(INC) -o $@ $(ROOT)/$(SRCDIR)/$(patsubst %.o,%.f90,$(notdir $@))

# include dynamically generated dependency file
-include $(ROOT)/$(DEPF)

# goodbye earthling!
