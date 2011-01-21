base := $(shell pwd)
log := ${base}/log
root := ${base}/root
absbin := ${base}/bin

TARGETS = $(basename $(shell find bin -name '[a-z]*.hs' -print))

all: $(TARGETS)

.PHONY: all always clean build dist doc browse install

WALL = -Wall
INCLUDES = -i$(HOME)/iterIO/ -ilib -ihs -icfg
GHC = ghc $(WALL) $(INCLUDES)
LIBS = -lz

always:
	@:

bin/%: always
	$(GHC) --make -threaded -i$(dir $@) $@.hs $(LIBS)

clean:
	rm -f $(TARGETS)
	find . \( -name '*~' -o -name '*.hi' -o -name '*.o' \) -print0 \
		| xargs -0 rm -f --

.PHONY: start
start: bin/app
	daemonize -v -c $(root) -a -e $(log)/err -o $(log)/out -p $(log)/pid -l $(log)/lock $(absbin)/app

.PHONY: stop
stop:
	kill `cat $(log)/pid`

.PHONY: restart
restart: bin/app
	-make stop
	sleep 1
	make start

