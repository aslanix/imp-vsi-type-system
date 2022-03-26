build:
	stack build 
	stack install --local-bin-path ./bin

run:
	./bin/imp-vsi-type-system

