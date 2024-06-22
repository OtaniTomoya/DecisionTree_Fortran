# Compiler
FC = gfortran

# Compiler flags
FFLAGS = -Wall -O2

# Modules
MODULES = modules/decision_tree_types.o \
          modules/decision_tree_utils.o \
          modules/decision_tree_io.o \
          modules/decision_tree_split.o \
          modules/decision_tree_metrics.o \
          modules/decision_tree_build.o

# Main program
MAIN = main.o

# Output executable
OUTPUT = decision_tree

# Build all
all: $(OUTPUT)

# Link the main program with the modules
$(OUTPUT): $(MODULES) $(MAIN)
	$(FC) $(FFLAGS) -o $(OUTPUT) $(MODULES) $(MAIN)

# Compile main program
main.o: main.f90
	$(FC) $(FFLAGS) -c main.f90

# Compile modules with dependencies
modules/decision_tree_types.o: modules/decision_tree_types.f90
	$(FC) $(FFLAGS) -c modules/decision_tree_types.f90 -o modules/decision_tree_types.o

modules/decision_tree_utils.o: modules/decision_tree_utils.f90 modules/decision_tree_types.o
	$(FC) $(FFLAGS) -c modules/decision_tree_utils.f90 -o modules/decision_tree_utils.o

modules/decision_tree_io.o: modules/decision_tree_io.f90 modules/decision_tree_types.o modules/decision_tree_utils.o
	$(FC) $(FFLAGS) -c modules/decision_tree_io.f90 -o modules/decision_tree_io.o

modules/decision_tree_split.o: modules/decision_tree_split.f90 modules/decision_tree_types.o modules/decision_tree_utils.o
	$(FC) $(FFLAGS) -c modules/decision_tree_split.f90 -o modules/decision_tree_split.o

modules/decision_tree_metrics.o: modules/decision_tree_metrics.f90 modules/decision_tree_types.o modules/decision_tree_utils.o
	$(FC) $(FFLAGS) -c modules/decision_tree_metrics.f90 -o modules/decision_tree_metrics.o

modules/decision_tree_build.o: modules/decision_tree_build.f90 modules/decision_tree_types.o modules/decision_tree_utils.o modules/decision_tree_split.o modules/decision_tree_metrics.o
	$(FC) $(FFLAGS) -c modules/decision_tree_build.f90 -o modules/decision_tree_build.o

# Clean up
clean:
	rm -f *.o *.mod $(OUTPUT)
	rm -f modules/*.o modules/*.mod
