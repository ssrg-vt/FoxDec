## =============================================================================
## == VARIABLES ================================================================
## =============================================================================

FOXDEC_DIR	:= /foxdec
XED_DIR		:= /xed/xed
STAGING_DIR	:= /staging
SRC_DIR		:= /src

CONFIG_DIR	:= /etc/foxdec
BIN_DIR		:= /usr/bin

FOXDEC_PREFIX	:= $(FOXDEC_DIR)/bin
XED_PREFIX		:= /xed/disasm

FOXDEC_BIN		:= $(FOXDEC_PREFIX)/foxdec-exe
XED_BIN			:= $(XED_PREFIX)/disasm
FRONTEND_PY		:= ci/twelf-foxdec.py

FOXDEC_DST		:= $(BIN_DIR)/foxdec
XED_DST			:= $(BIN_DIR)/foxdec-xed
FRONTEND_DST	:= $(BIN_DIR)/twelf-foxdec


## =============================================================================
## == BUILD XED ================================================================
## =============================================================================

.PHONY: xed-deps xed

xed-deps:
	cd $(XED_DIR) && ./mfile.py install

xed: xed-deps
	cd $(XED_DIR)
	gcc -shared -o libxed.so -fPIC \
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

install: install-foxdec install-frontend

install-foxdec:
	mkdir -p $(CONFIG_DIR)
	cp $(STAGING_DIR)/$(FOXDEC_BIN) $(FOXDEC_DST)
	cp $(SRC_DIR)/foxdec/config/config.dhall $(CONFIG_DIR)
	chmod +x $(FOXDEC_DST)

install-frontend:
	cp $(SRC_DIR)/$(FRONTEND_PY) $(FRONTEND_DST)
	chmod +x $(FRONTEND_DST)
