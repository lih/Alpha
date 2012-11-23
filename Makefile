all: bin/alpha

bin/alpha: $(shell find src -name '*.hs')
	cabal build

configure:
	cabal configure

run: all
	cd exec && ../bin/alpha < in

install: all
	cabal install

find:
	grep -Rn 'trace' src
stat:
	@echo -n 'Line total: ' 
	@find src -type f | xargs cat | wc -l
	@echo -n 'Total size (in bytes): '
	@find src -type f | xargs cat | wc -c
	@echo 'Biggest files (in LoC):'
	@find src -type f | xargs wc -l | head -n-1 | sort -nr | head


