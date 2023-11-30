CC = mpicc
CFLAGS += -O3
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/synchronous_or_asynchronous

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/synchronous_or_asynchronous: $(SRC_DIR)/synchronous_or_asynchronous.c
	$(CC) -o $@ $< $(CFLAGS)

clean:
	rm -f $(BIN_DIR)/*;
