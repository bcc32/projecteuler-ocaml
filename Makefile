BUILD_DIR=_build
CFLAGS=-warn-error,A,-safe-string
PKGS=core,core_bench,async,bignum,re2
SRC=src

all:
	corebuild                 \
		-cflags    $(CFLAGS)    \
		-I         $(SRC)       \
		-pkgs      $(PKGS)      \
		-build-dir $(BUILD_DIR) \
		main.native
	cp -f ./$(BUILD_DIR)/$(SRC)/main.native main.exe

clean:
	corebuild -build-dir $(BUILD_DIR) -clean
	rm -f main.exe

.PHONY: clean
