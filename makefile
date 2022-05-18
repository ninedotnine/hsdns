SHELL = /bin/sh

HSFLAGS := -dynamic -O1 -fmax-errors=2
CACHE := .cache

GHC_WARNS := -Wall -Wextra -Wmissing-exported-signatures -Widentities \
             -Wpartial-fields -Wredundant-constraints
GHC_EXTS := -XOverloadedStrings -XLambdaCase -XScopedTypeVariables \
            -XImportQualifiedPost -XBinaryLiterals -XNumericUnderscores \


GHC_FLAGS := -outputdir $(CACHE) -isrc/ \
			 $(GHC_WARNS) $(GHC_EXTS) $(HSFLAGS)


.PHONY: all
all: bin/hsdns-server bin/hsdns-client


SRCS_SERVER := $(shell find src/HSDNS/Server/ -name '*.hs')
SRCS_CLIENT := $(shell find src/HSDNS/Client/ -name '*.hs')
SRCS_COMMON := $(shell find src/HSDNS/Common/ -name '*.hs')

bin/hsdns-client: src/hsdns-client.hs $(SRCS_CLIENT)
bin/hsdns-server: src/hsdns-server.hs $(SRCS_SERVER)

bin/hsdns-client bin/hsdns-server: $(SRCS_COMMON) src/HSDNS/Settings.hs \
                                   | bin $(CACHE)
	ghc $(GHC_FLAGS) -o $@ $(patsubst bin/%,src/%.hs,$@)
	@rm -f $(CACHE)/Main.hi $(CACHE)/Main.o


bin $(CACHE):
	mkdir -p $@


.PHONY: test
test: all
	@test/integration-test.sh

.PHONY: check
check: test


.PHONY: clean
clean:
	rm -fr bin/ $(CACHE)

