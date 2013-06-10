Name:=alpha
Dirs:=alpha-src lib-src

all: bin/$(Name)

dist/setup-config: $(Name).cabal
	cabal configure
bin/$(Name): dist/setup-config $(shell find $(Dirs) -name '*.hs')
	cabal build

run: all
	cd exec && ../bin/$(Name) < in

clean:
	cabal clean
	find \( -name '.#*' -o -name '*~' \) -delete
mrproper: clean

debug:
	ghci -isrc -idist/build/autogen -pgmP cpphs -optP --hashes -optP --cpp src/formats.o -fllvm

install:
	cabal install
doc:
	cabal haddock --executables --internal

replaceExpr:='s/\bMy.Prelude\b/Misc/'

find:
	egrep -Rn 'flip' $(Dirs)
replace:
	find $(Dirs) -name '*.hs' | xargs sed -r $(replaceExpr)'g' -i
try-replace:
	find $(Dirs) -name '*.hs' | xargs sed -rn $(replaceExpr)'gp'
stat:
	@echo -n 'Line total: ' 
	@find $(Dirs) -type f | xargs cat | wc -l
	@echo -n 'Total size (in bytes): '
	@find $(Dirs) -type f | xargs cat | wc -c
	@echo 'Biggest files (in LoC):'
	@find $(Dirs) -type f | xargs wc -l | head -n-1 | sort -nr | head


