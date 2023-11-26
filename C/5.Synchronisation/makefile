CC = gcc-13
CFLAGS += -O3 -fopenmp
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/message_exchange

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/message_exchange: $(SRC_DIR)/message_exchange.c
	$(CC) -o $@ $< $(CFLAGS)
