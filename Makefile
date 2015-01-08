TARGET=xmonad-$(ARCH)-$(OS)
TARGET_DIR=$(HOME)/.xmonad
ARCH=$(shell arch)
OS=linux

.PHONY: all build-and-install diff

all: build-and-install diff

build-and-install:
	@cabal configure --bindir=${HOME}/.xmonad
	@cabal build
	@echo It\'s totally fine if there are warnings about the system search path below.
	@cabal copy
	@mv -v $(TARGET_DIR)/xmonad-javran $(TARGET_DIR)/$(TARGET)
	@rsync -uarcvv ./etc/ $(TARGET_DIR)/

clean:
	@cabal clean

diff:
	@echo "==== diff BEGIN ===="
	@-( diff -Nr ./etc/ ~/.xmonad/ | grep -Pv "^Binary files" | true)
	@echo "==== diff END ===="
