#######################################
# fintern   ---> internal files
# example
# uses subroutines
#######################################
F77 = gfortran --std=legacy 
RM = rm -f
EEXT =
OEXT = .o
DEXT = .csv
SHELL= bash
S= 100
TARGETS = fintern
.PHONY: clean all

all: fintern

fintern: fintern.f
	$(F77) $< -o $@



clean:
	$(RM) fintern

