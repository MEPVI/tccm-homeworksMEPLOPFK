# Makefile for Molecular Dynamics Program

# Compiler
FC = gfortran

# Compiler flags
FLAGS = -O2

# Directories
SRCDIR = src
OBJDIR = obj
BINDIR = bin

# Source and object files
SOURCES = $(SRCDIR)/MD_program.f90 $(SRCDIR)/MD_functions_corrected.f90
OBJECTS = $(OBJDIR)/MD_program.o $(OBJDIR)/MD_functions_corrected.o

# Executable name
EXEC = $(BINDIR)/MD_program

# Default rule
all: $(EXEC)

# Rule to build the executable
$(EXEC): $(OBJECTS)
	mkdir -p $(BINDIR)
	$(FC) $(FLAGS) -o $(EXEC) $(OBJECTS)

# Rule to compile MD_functions_corrected.f90
$(OBJDIR)/MD_functions_corrected.o: $(SRCDIR)/MD_functions_corrected.f90
	mkdir -p $(OBJDIR)
	$(FC) $(FLAGS) -c $(SRCDIR)/MD_functions_corrected.f90 -o $(OBJDIR)/MD_functions_corrected.o

# Rule to compile MD_program.f90
$(OBJDIR)/MD_program.o: $(SRCDIR)/MD_program.f90
	mkdir -p $(OBJDIR)
	$(FC) $(FLAGS) -c $(SRCDIR)/MD_program.f90 -o $(OBJDIR)/MD_program.o

# Clean rule to remove compiled files
clean:
	rm -rf $(OBJDIR) $(BINDIR)

# Phony targets
.PHONY: all clean
