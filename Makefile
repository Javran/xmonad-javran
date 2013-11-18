TARGET=xmonad-$(ARCH)-$(OS)
TARGET_TMP=$(TARGET)--temp
XMONAD_MAIN=xmonad.hs
ERROR_LOG=xmonad.errors
ARCH=$(shell arch)
OS=linux

all: $(TARGET)

$(TARGET): xmonad.hs
	@rm -vf $(ERROR_LOG)
	@ghc --make "xmonad.hs" \
		-O2 -ilib -fforce-recomp \
		-v0 -o $(TARGET_TMP) 2>&1 | tee $(ERROR_LOG) && \
	 mv -v $(TARGET_TMP) $(TARGET)

clean:
	@find . \
		-type d \
		-name .git \
		-prune \
		-or \
		-type f \
		\( -iname "*.hi" -or -iname "*.o" -or -executable \) \
		-exec rm -v {} \;
	@rm -vf $(ERROR_LOG)
