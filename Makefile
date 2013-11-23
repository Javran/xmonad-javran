TARGET=xmonad-$(ARCH)-$(OS)
TARGET_TMP=$(TARGET)--temp
XMONAD_MAIN=xmonad.hs
ERROR_LOG=xmonad.errors
ARCH=$(shell arch)
OS=linux

all: $(TARGET) StreamConvert

$(TARGET): xmonad.hs lib/JavranXMonad/Config.hs
	@rm -vf $(ERROR_LOG)
	@ghc --make "xmonad.hs" \
		-O2 -ilib -fforce-recomp \
		-v0 -o $(TARGET_TMP) 2>&1 | tee $(ERROR_LOG) && \
	 mv -v $(TARGET_TMP) $(TARGET)

StreamConvert: lib/JavranXMonad/StreamConvert.hs
	@ghc "lib/JavranXMonad/StreamConvert.hs" \
		-O2 -ilib -fforce-recomp \
		-o StreamConvert

clean:
	@find . \
		-type d \
		-name .git \
		-prune \
		-or \
		-name xmonad-init.sh \
		-prune \
		-or \
		-type f \
		\( -iname "*.hi" -or -iname "*.o" -or -executable \) \
		-exec rm -v {} \;
	@rm -vf $(ERROR_LOG)
