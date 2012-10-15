all: 
	cabal build
configure:
	cabal configure

find:
	find src | xargs egrep -n '[a-z]F\b.* =' 
stat:
	@echo -n 'Line total: ' 
	@find src -type f | xargs cat | wc -l
	@echo -n 'Total size (in bytes): '
	@find src -type f | xargs cat | wc -c
	@echo 'Biggest files (number of lines):'
	@find src -type f | xargs wc -l | head -n-1 | sort -nr | head

