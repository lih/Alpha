all: bin/alpha

bin/alpha: $(shell find src -name '*.hs')
	cabal build

run: all
	cd exec && ../bin/alpha < in

clean:
	rm -rf dist/build
mrproper: clean
debug:
	ghci -isrc -idist/build/autogen -pgmP cpphs -optP --hashes -optP --cpp src/formats.o -fllvm

install: all
	cabal install
doc:
	cabal haddock --executables

replaceExpr:='s/\bMy.Prelude\b/Misc/'

find:
	egrep -Rn 'flip' src
replace:
	find src -name '*.hs' | xargs sed -r $(replaceExpr)'g' -i
try-replace:
	find src -name '*.hs' | xargs sed -rn $(replaceExpr)'gp'
stat:
	@echo -n 'Line total: ' 
	@find src -type f | xargs cat | wc -l
	@echo -n 'Total size (in bytes): '
	@find src -type f | xargs cat | wc -c
	@echo 'Biggest files (in LoC):'
	@find src -type f | xargs wc -l | head -n-1 | sort -nr | head


