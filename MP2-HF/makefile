# Compiler and flags
CC = gcc
CFLAGS = -Wall -O2 -I/Users/marioperez/GITHUBPRoject/trexio-2.5.0/include/
LDFLAGS = -L/Users/marioperez/GITHUBPRoject/trexio-2.5.0/src/ -ltrexio

# Targets and source files
TARGET = main
SRC = src/main.c src/HF.c src/MP2.c
OBJ = $(SRC:.c=.o)

# Default rule
all: $(TARGET)

# Link object files to create the executable
$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) -o $(TARGET) $(LDFLAGS)

# Compile source files into object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up build files
clean:
	rm -f $(OBJ) $(TARGET)

# Run the program
run: $(TARGET)
	./$(TARGET)
