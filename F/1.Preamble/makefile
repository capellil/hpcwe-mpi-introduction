CC = mpifort
CFLAGS += 
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/preamble

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/preamble: $(SRC_DIR)/preamble.f08
	$(CC) -o $@ $< $(CFLAGS)

clean:
	rm -f $(BIN_DIR)/*;