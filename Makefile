TARGET=xmonad-$(ARCH)-$(OS)
TARGET_DIR=$(HOME)/.xmonad
ARCH=$(shell arch)
OS=linux

build-and-install:
	@cabal configure --bindir=${HOME}/.xmonad
	@cabal build
	@echo It\'s totally fine if there are warnings about the system search path below.
	@cabal copy
	@mv -v $(TARGET_DIR)/xmonad-javran $(TARGET_DIR)/$(TARGET)
	@cp -v etc/* $(TARGET_DIR)/

clean:
	@cabal clean
