CC = mpicc
CFLAGS += -O3
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/collectives

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/collectives: $(SRC_DIR)/collectives.c
	$(CC) -o $@ $< $(CFLAGS)

clean:
	rm -f $(BIN_DIR)/*;

