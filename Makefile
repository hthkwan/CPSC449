CC=ghc
# test
build:
	$(CC) Apoc.hs

.PHONY: clean
clean:
	rm -rf *.hi *.o Apoc
