# Build examples
BINARIES=ParseAndPrint ScanFile SearchDef TypeCheck ComputeSize DumpAst PrintTokens

PROJECT_DIR=..
BIN_DIR=.
INPLACE_PACKAGE_CONF=$(PROJECT_DIR)/dist/package.conf.inplace

all: build
# recommended to use cabal sandbox with local source ../
build: $(INPLACE_PACKAGE_CONF)
	if [ ! -e .cabal-sandbox ] ; then \
	  cabal install --package-db $(INPLACE_PACKAGE_CONF) --dependencies-only; \
	  cabal configure --package-db $(INPLACE_PACKAGE_CONF); \
        fi
	cabal build
	for b in $(BINARIES) ; do cp dist/build/$$b/$$b $(BIN_DIR) ; done

demo_compute_size: ComputeSize
	gcc -DDEBUG compute_size.c -o compute_size.bin && ./compute_size.bin
	./ComputeSize 'comp' compute_size.c | gcc -x c -o compute_size_hs.bin - && \
	  ./compute_size_hs.bin
ComputeSize:
	echo "Please run make build first" >&2
	exit 1

clean:
	rm -f $(addprefix $(BIN_DIR)/, $(BINARIES)) compute_size.bin compute_size_hs.bin
	cabal clean

../dist/package.conf.inplace:
	echo "Please run cd .. && cabal build first"
	exit 1
