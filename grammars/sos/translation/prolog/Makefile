

# All silver files in included grammars, to be included as dependencies
HOST_SOURCES=$(shell find ../../core -name "*.sv" -print0 | xargs -0)
GRAMMAR_SOURCES=$(shell find . -name '*.sv' -print0 | xargs -0)

sos.core.jar: $(HOST_SOURCES) $(GRAMMAR_SOURCES)
	silver -I ../../../ sos:translation:prolog

mwda:
	silver --clean --mwda -I ../../../ sos:translation:prolog

