TARGET=xmonad-$(ARCH)-$(OS)
TARGET_DIR=$(HOME)/.xmonad
ARCH=$(shell arch)
OS=linux

LOCK_FILE=.build-protect.lock

.PHONY: all build-and-install diff

all: build-and-install diff

build-and-install:
	@if [ ! -e $(LOCK_FILE) ] ; \
	then \
	     echo $$PPID > $(LOCK_FILE) ; \
	else \
	     echo Lock file exists, aborting ... ; \
	     echo Remove "$(LOCK_FILE)" or use "make clean" to retry. ; \
             exit 1 ; \
	fi;
	@cabal configure --bindir=${HOME}/.xmonad
	@cabal build
	@echo It\'s totally fine if there are warnings about the system search path below.
	@cabal copy
	@mv -v $(TARGET_DIR)/xmonad-javran $(TARGET_DIR)/$(TARGET)
	@rsync -uarcvv ./etc/ $(TARGET_DIR)/
	@rm $(LOCK_FILE)

clean:
	@rm -vf $(LOCK_FILE)
	@cabal clean

diff:
	@echo "==== diff BEGIN ===="
	@-( diff -Nr ./etc/ ~/.xmonad/ | grep -Pv "^Binary files" | true)
	@echo "==== diff END ===="
