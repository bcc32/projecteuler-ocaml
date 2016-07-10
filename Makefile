all:
	corebuild main.native -cflags -warn-error,A
	mv main.native main.exe				# TODO a better way to do this?

clean:
	rm -rf _build main.exe

.PHONY: clean
