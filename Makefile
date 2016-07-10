BUILD_DIR=_build
CFLAGS=-warn-error,A
SRC=src

all:
	corebuild -cflags $(CFLAGS) -I $(SRC) -build-dir $(BUILD_DIR) main.native
	ln -fs ./$(BUILD_DIR)/$(SRC)/main.native main.exe # TODO a better way to do this?

clean:
	corebuild -build-dir $(BUILD_DIR) -clean
	rm main.exe

.PHONY: clean
