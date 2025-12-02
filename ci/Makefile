## =============================================================================
## == VARIABLES ================================================================
## =============================================================================

FOXDEC_DIR	:= /foxdec
XED_DIR		:= /xed/xed
STAGING_DIR	:= /staging
SRC_DIR		:= /src

CONFIG_DIR	:= /etc/foxdec
BIN_DIR		:= /usr/bin
LIB_DIR		:= /usr/local/lib

FOXDEC_PREFIX	:= $(FOXDEC_DIR)/bin
XED_PREFIX		:= $(XED_DIR)/obj/wkit/lib

FOXDEC_BIN		:= $(FOXDEC_PREFIX)/foxdec-exe
FOXED_LIB		:= libfoxed.so
XED_LIB			:= libxed.so

FOXDEC_DST		:= $(BIN_DIR)/foxdec
FOXED_DST		:= $(LIB_DIR)/libfoxed.so
XED_DST			:= $(LIB_DIR)/libxed.so


## =============================================================================
## == BUILD XED ================================================================
## =============================================================================

.PHONY: xed-deps xed

xed-deps:
	cd $(XED_DIR) && ./mfile.py --shared install

xed: xed-deps
	gcc -c disasm1.c -o disasm1.o -fPIC \
		-I $(XED_DIR)/include/public/xed/ \
		-I $(XED_DIR)/obj/wkit/include/xed/

	gcc -shared -o $(FOXED_LIB) -fPIC \
		-L $(XED_PREFIX) \
		-Wl,--no-undefined \
		disasm1.o -lxed

## =============================================================================
## == BUILD FOXDEC =============================================================
## =============================================================================

.PHONY: foxdec-deps foxdec

foxdec-deps:
	cabal update
	cabal build --only-dependencies

foxdec:
	cabal build
	cabal install \
		--installdir=$(FOXDEC_PREFIX) \
		--overwrite-policy=always \
		--install-method=copy

## =============================================================================
## == INSTALL ==================================================================
## =============================================================================

.PHONY: install install-foxdec install-xed

install: install-foxdec install-xed

install-foxdec:
	mkdir -p $(CONFIG_DIR)
	cp $(STAGING_DIR)/$(FOXDEC_BIN) $(FOXDEC_DST)
	cp $(SRC_DIR)/foxdec/config/config.dhall $(CONFIG_DIR)
	chmod +x $(FOXDEC_DST)

install-xed:
	cp $(STAGING_DIR)/$(FOXED_LIB) $(FOXED_DST)
	cp $(STAGING_DIR)/$(XED_LIB)   $(XED_DST)
