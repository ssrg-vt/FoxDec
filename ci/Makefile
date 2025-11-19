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

FOXDEC_BIN		:= $(FOXDEC_PREFIX)/foxdec-exe
XED_LIB			:= libxed.so
FRONTEND_PY		:= ci/twelf-foxdec.py

FOXDEC_DST		:= $(BIN_DIR)/foxdec
XED_DST			:= $(LIB_DIR)/libxed.so
FRONTEND_DST	:= $(BIN_DIR)/twelf-foxdec


## =============================================================================
## == BUILD XED ================================================================
## =============================================================================

.PHONY: xed-deps xed

xed-deps:
	cd $(XED_DIR) && ./mfile.py install

xed: xed-deps
	gcc -shared -o $(XED_LIB) -fPIC \
		-I $(XED_DIR)/include/public/xed/ \
		-I $(XED_DIR)/obj/wkit/include/xed/ \
		-L $(XED_DIR)/obj/wkit/lib/ \
		-lxed \
		disasm1.c

## =============================================================================
## == BUILD FOXDEC =============================================================
## =============================================================================

.PHONY: foxdec-deps foxdec

foxdec-deps:
	cabal update
	cabal v2-build --only-dependencies

foxdec:
	cabal v2-build
	cabal v2-install \
		--installdir=$(FOXDEC_PREFIX) \
		--overwrite-policy=always \
		--install-method=copy

## =============================================================================
## == INSTALL ==================================================================
## =============================================================================

.PHONY: install install-foxdec install-xed install-frontend

install: install-foxdec install-frontend install-xed

install-foxdec:
	mkdir -p $(CONFIG_DIR)
	cp $(STAGING_DIR)/$(FOXDEC_BIN) $(FOXDEC_DST)
	cp $(SRC_DIR)/foxdec/config/config.dhall $(CONFIG_DIR)
	chmod +x $(FOXDEC_DST)

install-xed:
	cp $(STAGING_DIR)/$(XED_LIB) $(XED_DST)

install-frontend:
	cp $(SRC_DIR)/$(FRONTEND_PY) $(FRONTEND_DST)
	chmod +x $(FRONTEND_DST)
