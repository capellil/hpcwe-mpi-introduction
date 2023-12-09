CC = mpicc
CFLAGS += -O3
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/nonblocking

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/nonblocking: $(SRC_DIR)/nonblocking.c
	$(CC) -o $@ $< $(CFLAGS)

clean:
	rm -f $(BIN_DIR)/*;

