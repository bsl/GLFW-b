GHC       := ghc -ignore-dot-ghci
GCCFLAGS  := $(shell $(GHC) --info | $(GHC) -e "fmap read getContents >>= \
             putStrLn . unwords . read . Data.Maybe.fromJust . lookup     \
             \"Gcc Linker flags\"")
FRAMEWORK := -framework Cocoa -framework OpenGL
GLFW_FLAG := $(GCCFLAGS) -O2 -msse2 -fno-common -Iglfw/include -Iglfw/src $(CFLAGS)
SRC_DIR   := glfw/src
BUILD_DIR := build

OBJ_C_SRC := $(wildcard $(SRC_DIR)/*.m)
C_SRC     := $(wildcard $(SRC_DIR)/cocoa_*.c)
GLFW_SRC  :=               \
  $(SRC_DIR)/clipboard.c   \
  $(SRC_DIR)/context.c     \
  $(SRC_DIR)/gamma.c       \
  $(SRC_DIR)/glx_context.c \
  $(SRC_DIR)/init.c        \
  $(SRC_DIR)/input.c       \
  $(SRC_DIR)/joystick.c    \
  $(SRC_DIR)/monitor.c     \
  $(SRC_DIR)/time.c        \
  $(SRC_DIR)/window.c
OBJS      := $(addprefix $(BUILD_DIR)/, $(OBJ_C_SRC:.m=.o) $(C_SRC:.c=.o))

all: $(BUILD_DIR)/libglfw.dylib

$(BUILD_DIR)/libglfw.dylib: $(OBJS)
	$(CC) -dynamiclib -Wl,-single_module -compatibility_version 1 \
        -current_version 1                                      \
        $(GLFW_FLAG) -o $@ $(OBJS) $(GLFW_SRC) $(FRAMEWORK)

.PHONY: $(BUILD_DIR)/$(SRC_DIR)/.build-tag

$(BUILD_DIR)/$(SRC_DIR)/.build-tag:
	mkdir -p $(BUILD_DIR)/$(SRC_DIR)
	touch $@

$(OBJS): $(BUILD_DIR)/$(SRC_DIR)/.build-tag

$(BUILD_DIR)/%.o: %.c
	$(CC) -c $(GLFW_FLAG) $< -o $@
$(BUILD_DIR)/%.o: %.m
	$(CC) -c $(GLFW_FLAG) $< -o $@

.PHONY: clean
clean:
	$(RM) -rf $(BUILD_DIR)
