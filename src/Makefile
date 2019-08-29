FC = gfortran
CC = g++

ifdef DEBUG
  DLLFLAGS=
  DEBUGFLAG=-gdwarf-2
else
  DLLFLAGS=-s
  DEBUGFLAG=
endif
FCFLAGS = -O2 $(DEBUGFLAG) -mtune=generic

FILEf_src := $(wildcard *.f90)   
FILEf_mod := $(patsubst %f90, %mod, $(FILEf_src)) 
FILEf_obj := $(patsubst %f90, %o, $(FILEf_src)) 

FILEc_src := $(wildcard *.c)
FILEc_obj := $(patsubst %c, %o, $(FILEc_src)) 

OBJS := mo_julian.o mo_percentile.o mo_smi_constants.o mo_sort.o mo_utils.o  
SRCS := $(patsubst %o, %f90, $(OBJS)) 
MODS := $(patsubst %o, %mod, $(OBJS)) 
# ALL:FILEf_obj,FILEc_obj

# echo $(FC) $(FCFLAGS) -c $^

# global.mod: global.o global.f90
#    ifort -c global.f90
# global.o: global.f90
#    ifort -c global.f90

ALL: mo_drought_evaluation.o register_routines.o

# all: $(SHLIB)
mo_drought_evaluation.o: mo_drought_evaluation.f90 \
	InputOutput.o mo_percentile.o mo_sort.o mo_smi_constants.o mo_utils.o 
	$(FC) $(FCFLAGS) -c -o $@ $<

InputOutput.o: InputOutput.f90 mo_kind.o mo_global_variables.o
	$(FC) $(FCFLAGS) -c -o $@ $<

mo_global_variables.o: mo_global_variables.f90 mo_kind.o mo_julian.o
	$(FC) $(FCFLAGS) -c -o $@ $<

# $(MODS): $(SRCS) ${OBJS} mo_kind.mod
# 	$(FC) $(FCFLAGS) -c -o $@ $<
$(OBJS): $(SRCS)
	$(FC) $(FCFLAGS) -c $^
# -o $@ 
# $(MODS): $(SRCS) mo_kind.o
# 	$(FC) $(FCFLAGS) -c -o $@ $<

mo_kind.o: mo_kind.f90
	$(FC) $(FCFLAGS) -c $<

register_routines.o: register_routines.c
	$(CC) $(CFLAGS) -c $<

# echo ${SRCS}, ${OBJS}
# @echo "#" $(FC) $(FCFLAGS) -c $< mo_kind.o
# mo_global_variables.o: mo_global_variables.f90 mo_kind.o
# 	# echo $(FC) $(FCFLAGS) -c $<
# 	$(FC) $(FCFLAGS) -c $<
# # mo_utils.o

# $(TARGET):$(objects)
# 	 $(FC) $(FCFLAGS) $(objects) -o $@

clean:  
	rm -f *.o *.mod $(TARGET)

# $@    the file name of the target
# $<    the name of the first prerequisite (i.e., dependency)
# $^    the names of all prerequisites (i.e., dependencies)
# $(@D)    the directory part of the target
# $(@F)    the file part of the target
# $(<D)    the directory part of the first prerequisite (i.e., dependency)
# $(<F)    the file part of the first prerequisite (i.e., dependency)
