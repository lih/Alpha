Out:=Alpha
Out_Args:=-Ssrc -Rrunes -i <<< "lang_Macros" | tee out

Flags:=obj/writeElf.o # -fwarn-incomplete-patterns

bin/$(Out): obj/writeElf.o
obj/writeElf.o: src/Alpha/writeElf.c src/Alpha/writeElf.h
	mkdir -p obj
	gcc -c $< -o $@

time: all
	cd exec && time ../bin/$(Out) $(Out_Args)

find:
	findSrc src | xargs egrep -n 'debug' 
stat:
	@echo -n 'Line total: ' 
	@findSrc src | xargs cat | wc -l
	@echo -n 'Total size (in bytes): '
	@findSrc src | xargs cat | wc -c
	@echo 'Biggest files (number of lines):'
	@findSrc src | xargs wc -l | head -n-1 | sort -nr | head

Name:=Alpha
Dest:=AlphaLang
install-alpha: all
	cd .. && \
	cp $(Name)/bin/Alpha $(Dest)/html/downloads/Alpha_x86_64 && \
	cp $(Name)/misc/alpha-mode.el $(Dest)/html/downloads/ && \
	./Common/bundle $(Name) && mv $(Name).tgz $(Dest)/html/downloads/Alpha.tgz

include Haskell.mk
