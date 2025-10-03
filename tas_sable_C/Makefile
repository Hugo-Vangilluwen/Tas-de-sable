BUILD_DIR := ./build
TARGET := sandpile

SRCS := $(wildcard *.c)
OJBS := $(SRCS:%.c=$(BUILD_DIR)/%.o)

CFLAGS :=

all: $(OJBS)
	gcc $(OJBS) -o $(BUILD_DIR)/$(TARGET) $(CFLAGS)

$(BUILD_DIR)/%.o: %.c
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -c $< -o $@


.PHONY: clean execute debug

clean:
	rm -r $(BUILD_DIR)

execute:
	./$(TARGET)

debug:
	make --always-make CFLAGS=-g
