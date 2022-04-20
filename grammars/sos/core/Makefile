

# All silver files in included grammars, to be included as dependancies
GRAMMAR_SOURCES=$(shell find . -name '*.sv' -print0 | xargs -0)

sos.core.jar: $(GRAMMAR_SOURCES)
	silver -I ../../ sos:core

mwda:
	silver --clean --mwda -I ../../ sos:core

