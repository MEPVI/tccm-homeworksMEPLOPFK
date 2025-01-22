# Compiler
FC = gfortran

# Compiler flags
FFLAGS = -O2 -Wall

# Directories
BINDIR = ../bin
OBJDIR = ../obj

# Source files
SOURCES = MD_functions_corrected.f90 MD_program.f90

# Object files
OBJECTS = $(addprefix $(OBJDIR)/, $(SOURCES:.f90=.o))

# Executable name
EXEC = $(BINDIR)/MD_program

# Default target
all: $(EXEC)

# Linking
$(EXEC): $(OBJECTS)
	@mkdir -p $(BINDIR)
	$(FC) $(FFLAGS) -o $@ $(OBJECTS)

# Compilation
$(OBJDIR)/%.o: %.f90
	@mkdir -p $(OBJDIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(OBJECTS) $(EXEC)
	rm -rf $(BINDIR) $(OBJDIR)

# Phony targets
.PHONY: all clean

