GCCFLAGS  := $(shell ghc --info | ghc -e "fmap read getContents >>=   \
             putStrLn . unwords . read . Data.Maybe.fromJust . lookup \
             \"Gcc Linker flags\"")
FRAMEWORK := -framework Cocoa -framework OpenGL
GLFW_FLAG := $(GCCFLAGS) -O2 -msse2 -fno-common -Iglfw/include -Iglfw/lib    \
             -Iglfw/lib/cocoa $(CFLAGS)
SRC_DIR   := glfw/lib/cocoa
GLFW_DIR  := glfw/lib
BUILD_DIR := build

OBJ_C_SRC := $(wildcard $(SRC_DIR)/*.m)
C_SRC     := $(wildcard $(SRC_DIR)/*.c)
GLFW_SRC  := $(wildcard $(GLFW_DIR)/*.c)
OBJS      := $(addprefix $(BUILD_DIR)/, $(OBJ_C_SRC:.m=.o) $(C_SRC:.c=.o))

all: $(BUILD_DIR)/libglfw.dylib

$(BUILD_DIR)/libglfw.dylib: $(OBJS)
	$(CC) -dynamiclib -Wl,-single_module -compatibility_version 1       \
        -current_version 1                                            \
        $(GLFW_FLAG) -o $@ $(OBJS) $(GLFW_SRC) $(FRAMEWORK)

.PHONY: $(BUILD_DIR)/$(SRC_DIR)/.build-tag

$(BUILD_DIR)/$(SRC_DIR)/.build-tag:
	mkdir -p $(BUILD_DIR)/$(SRC_DIR)
	touch $@

$(OBJS): $(BUILD_DIR)/$(SRC_DIR)/.build-tag

$(BUILD_DIR)/%.o : %.c
	$(CC) -c $(GLFW_FLAG) $< -o $@
$(BUILD_DIR)/%.o : %.m
	$(CC) -c $(GLFW_FLAG) $< -o $@

.PHONY: clean
clean:
	$(RM) -rf $(BUILD_DIR)
