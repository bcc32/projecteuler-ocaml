all:
	corebuild main.native -cflags -warn-error,A -build-dir bin -I src
	ln -fs ./bin/src/main.native main.exe # TODO a better way to do this?

clean:
	rm -rf bin main.exe

.PHONY: clean
