OUT = out
OBJ = $(OUT)/obj
WEB = $(OUT)/web

TARGETS = $(OUT)/stenoforth $(OUT)/stenoforth.exe $(WEB)/stenoforth.js $(WEB)/stenoforth.html

WIN_ARCH=i686

CC = clang
#CFLAGS = -Wall -Werror \
         -g
CFLAGS = -Wall -Werror \
         -O2 \
         -fno-exceptions \
         -s \
         -fomit-frame-pointer \
         -fno-stack-protector \
         -fno-ident -Wl,--build-id=none \
         -ffunction-sections -fdata-sections \
         -Wl,--gc-sections \
         -fmerge-all-constants
STRIP_ARGS = -S \
             --strip-unneeded \
             --remove-section=.note.gnu.gold-version \
             --remove-section=.comment \
             --remove-section=.note \
             --remove-section=.note.gnu.build-id \
             --remove-section=.note.ABI-tag
WINFLAGS = -mwindows -luser32

all: $(TARGETS)

$(OUT):
	mkdir -p $@

$(OBJ):
	mkdir -p $@

$(WEB):
	mkdir -p $@

HEADERS = stenoforth.h

$(OBJ)/extract: extract.c $(HEADERS) | $(OBJ)
	$(CC) $(CFLAGS) $< -o $@

$(OBJ)/oplist.txt: $(OBJ)/extract | $(OBJ)
	$< ops >$@

$(OBJ)/boot.dat: compile.js $(OBJ)/oplist.txt core.fs | $(OBJ)
	nodejs $^ >$@

$(OBJ)/ops.txt: $(OBJ)/extract | $(WEB)
	$< js >$@

$(WEB)/stenoforth.js: $(OBJ)/ops.txt $(OBJ)/boot.dat webify.js | $(WEB)
	nodejs webify.js $(OBJ)/ops.txt $(OBJ)/boot.dat >$@

$(WEB)/stenoforth.html: stenoforth.html | $(WEB)
	cp $< $@

$(WEB)/boot.dat: $(OBJ)/boot.dat | $(WEB)
	cp $< $@

BOOT = $(OBJ)/boot.c
SOURCES_COMMON = stenoforth.c stenoforth_loader.c $(BOOT)
SOURCES_POSIX = $(SOURCES_COMMON) stenoforth_posix.c
SOURCES_WINDOWS = $(SOURCES_COMMON) stenoforth_windows.c

$(OUT)/stenoforth: $(SOURCES_POSIX) $(HEADERS) | $(OUT)
	$(CC) $(CFLAGS) $(SOURCES_POSIX) -o $@
	strip $(STRIP_ARGS) $@

RESOURCES = $(OBJ)/stenoforth_res.o

$(OUT)/stenoforth.exe: $(SOURCES_WINDOWS) $(HEADERS) $(RESOURCES) | $(OUT)
	$(WIN_ARCH)-w64-mingw32-gcc $(CFLAGS) $(WINFLAGS) $(SOURCES_WINDOWS) $(RESOURCES) -o $@
	strip $(STRIP_ARGS) $@

$(OBJ)/stenoforth16x16.png: stenoforth.png | $(OBJ)
	convert -resize 16x16 $< $@

$(OBJ)/stenoforth32x32.png: stenoforth.png | $(OBJ)
	convert -resize 32x32 $< $@

$(OBJ)/stenoforth48x48.png: stenoforth.png | $(OBJ)
	convert -resize 48x48 $< $@

$(OBJ)/stenoforth256x256.png: stenoforth.png | $(OBJ)
	convert -resize 256x256 $< $@

ICON_SIZES = $(OBJ)/stenoforth256x256.png \
             $(OBJ)/stenoforth48x48.png \
             $(OBJ)/stenoforth32x32.png \
             $(OBJ)/stenoforth16x16.png

$(OBJ)/stenoforth.ico: $(ICON_SIZES)
	convert $^ $< $@

$(RESOURCES): stenoforth.rc $(OBJ)/stenoforth.ico
	$(WIN_ARCH)-w64-mingw32-windres $< $@

$(OBJ)/boot.c: $(OBJ)/boot.dat | $(OBJ)
	echo "__attribute__ ((section (\".forth\"))) unsigned char boot_dat[] = {" >$@
	cat $< | xxd -i >>$@
	echo "};" >>$@

clean:
	rm -rf $(OUT)

test: $(OUT)/stenoforth
	./test.sh

# objcopy -O binary -j .forth out/stenoforth.exe boot.out
# objcopy --update-section .forth=alternate out/stenoforth.exe out/revised.exe
