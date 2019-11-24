.PHONY : all clean hindent hlint test

HINDENT = hindent --line-length 76 --sort-imports
HLINT = hlint

test : all
	stack test

all :
	stack build

hindent :
	find src test -name '*.hs' -exec $(HINDENT) \{} \;

hlint :
	hlint src test

clean : # hindent
	stack clean
	find . -name '*~' -delete
	find . -name '#*' -delete
