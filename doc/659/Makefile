.PHONY: run clean

ARTIFACTS := sum

run: 659.sh sum
	./659.sh

sum: sum.hs
	ghc $^

clean:
	rm -rf $(ARTIFACTS)
	rm -rf *.hi *.o
