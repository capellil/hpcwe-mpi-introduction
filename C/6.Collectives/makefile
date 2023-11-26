CC = gcc-13
CFLAGS += -O3 -fopenmp
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/collectives

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/collectives: $(SRC_DIR)/collectives.c
	$(CC) -o $@ $< $(CFLAGS)
