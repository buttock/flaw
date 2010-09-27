
TARGETS = $(basename $(shell find bin -name '[a-z]*.hs' -print))

all: $(TARGETS)

.PHONY: all always clean build dist doc browse install

WALL = -Wall -Werror
INCLUDES = -i$(HOME)/iterIO/
GHC = ghc $(WALL) $(INCLUDES)

always:
	@:

bin/%: always
	$(GHC) --make -i$(dir $@) $@.hs

clean:
	rm -f $(TARGETS)
	find . \( -name '*~' -o -name '*.hi' -o -name '*.o' \) -print0 \
		| xargs -0 rm -f --

