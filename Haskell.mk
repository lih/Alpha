.PHONY: all run mrproper clean doc bin/$(Out)

Flags+= -outputdir obj -isrc

all: bin/$(Out)
run: all
	@mkdir -p exec
	@cd exec && ../bin/$(Out) $(Out_Args)

bin/$(Out): 
	@mkdir -p bin
	@ghc $(Flags) --make -o $@ src/$(Out).hs

clean:
	@rm -rf obj
mrproper: clean
	@rm -f bin/$(Out)
	@find -name '.*~' | xargs -d'\n' rm -rf 

doc:
	@mkdir -p doc
	@haddock --html -odoc $(Flags:%=--optghc="%") src/$(Out).hs