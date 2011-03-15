FRAMEWORK := -framework AGL -framework Cocoa -framework OpenGL
CFLAGS    := -O2 -Iglfw/include -Iglfw/lib -Iglfw/lib/cocoa
SRC_DIR   := glfw/lib/cocoa
GLFW_DIR  := glfw/lib
BUILD_DIR := build

OBJ_C_SRC := $(wildcard $(SRC_DIR)/*.m)
C_SRC     := $(wildcard $(SRC_DIR)/*.c)
GLFW_SRC  := $(wildcard $(GLFW_DIR)/*.c)
OBJS      := $(addprefix $(BUILD_DIR)/, $(OBJ_C_SRC:.m=.o) $(C_SRC:.c=.o))

all: $(BUILD_DIR)/libglfw.a $(BUILD_DIR)/libglfw.dylib

$(BUILD_DIR)/libglfw.dylib: $(OBJS)
	$(CC) -dynamiclib $(CFLAGS) -o $@ $(OBJS) $(GLFW_SRC) $(FRAMEWORK)

$(BUILD_DIR)/libglfw.a: $(OBJS)
	ar -r -s $@ $(OBJS)

$(BUILD_DIR)/$(SRC_DIR)/.build-tag:
	mkdir -p $(BUILD_DIR)/$(SRC_DIR)
	touch $@

$(OBJS): $(BUILD_DIR)/$(SRC_DIR)/.build-tag

$(BUILD_DIR)/%.o : %.c
	$(CC) -c $(CFLAGS) $< -o $@
$(BUILD_DIR)/%.o : %.m
	$(CC) -c $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	$(RM) -rf $(BUILD_DIR)
