
CC = gcc

CFLAGS = -Wall -Iinclude -I/opt/homebrew/opt/openblas/include
LDFLAGS = -L/opt/homebrew/lib -ltrexio

SRC = main.c data-reader.c
OBJ = main.o data-reader.o

CC = gcc 
CFLAGS = -Wall -Iinclude -I/opt/homebrew/opt/openblas/include
LDFLAGS = -L/opt/homebrew/lib -ltrexio

SRC = main.c data-reader.c HF.c
OBJ = main.o data-reader.o HF.o
EXEC = main

all: $(EXEC)

$(EXEC): $(OBJ)
	$(CC) $(OBJ) -o $(EXEC) $(LDFLAGS)

main.o: main.c
	$(CC) $(CFLAGS) -c main.c -o main.o

data_reader.o: data_reader.c reader-data.h
	$(CC) $(CFLAGS) -c data_reader.c -o data_reader.o

HF.o: HF.c HF.h 
	$(CC) $(CFLAGS) -c HF.c -o HF.o
clean:
	rm -f $(OBJ) $(EXEC)
