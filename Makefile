GHC       := ghc
WASM_GHC  := wasm32-wasi-ghc
SRC       := src/Main.hs
WASM_SRC  := src/MainWasm.hs
SRCS      := $(wildcard src/*.hs src/CPU/*.hs)
OUTDIR    := web
WASM_OUT  := $(OUTDIR)/computersystem.wasm
NATIVE    := /tmp/cputest
GHC_WASM_ENV := $(HOME)/.ghc-wasm/env

# ── ネイティブビルド (開発・テスト用) ──────────
.PHONY: native
native: $(NATIVE)
$(NATIVE): $(SRCS)
	$(GHC) --make -isrc $(SRC) -o $(NATIVE) -O2
	@echo "✓ Native build OK"

.PHONY: test
test: native
	$(NATIVE)

# ── WASM ビルド ───────────────────────────────
.PHONY: wasm
wasm:
	@if ! command -v $(WASM_GHC) > /dev/null 2>&1; then \
	  if [ -f "$(GHC_WASM_ENV)" ]; then \
	    . "$(GHC_WASM_ENV)" && $(MAKE) _wasm_build; \
	  else \
	    echo ""; \
	    echo "ERROR: $(WASM_GHC) が見つかりません。"; \
	    echo ""; \
	    echo "  make install-wasm-ghc  でインストールしてから"; \
	    echo "  source ~/.ghc-wasm/env && make wasm  を実行してください。"; \
	    echo ""; \
	    exit 1; \
	  fi \
	else \
	  $(MAKE) _wasm_build; \
	fi

.PHONY: _wasm_build
_wasm_build: $(SRCS)
	$(WASM_GHC) --make -isrc $(WASM_SRC) \
	  -no-hs-main \
	  src/wasm_stub.c \
	  -o $(WASM_OUT) \
	  -O2 \
	  -optl-Wl,--export=hs_init \
	  -optl-Wl,--export=hs_get_layout \
	  -optl-Wl,--export=hs_get_state \
	  -optl-Wl,--export=hs_get_wires \
	  -optl-Wl,--export=hs_step \
	  -optl-Wl,--export=hs_reset \
	  -optl-Wl,--export=hs_load_asm \
	  -optl-Wl,--export=malloc \
	  -optl-Wl,--export=free
	@echo "✓ WASM build OK: $(WASM_OUT)"

# ── GHC WASM バックエンドのインストール ─────────
.PHONY: install-wasm-ghc
install-wasm-ghc:
	@echo ">>> GHC WASM バックエンドをインストールします (~/.ghc-wasm/)"
	curl -fsSL \
	  https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/raw/master/bootstrap.sh \
	  | FLAVOUR=9.12 sh
	@echo ""
	@echo "✓ インストール完了。次のコマンドを実行してください:"
	@echo ""
	@echo "  source ~/.ghc-wasm/env"
	@echo "  make wasm"
	@echo ""

# ── 開発サーバー ──────────────────────────────
.PHONY: serve
serve:
	python3 -m http.server 8080 --directory $(OUTDIR)

# ── cabal ────────────────────────────────────
.PHONY: cabal-build cabal-run
cabal-build:
	cabal build
cabal-run:
	cabal run computersystem

# ── クリーン ──────────────────────────────────
.PHONY: clean
clean:
	rm -f $(NATIVE) $(WASM_OUT) src/*.o src/*.hi src/CPU/*.o src/CPU/*.hi

.PHONY: help
help:
	@echo "使い方:"
	@echo "  make test              ネイティブビルド & テスト実行"
	@echo "  make install-wasm-ghc  GHC WASM バックエンドをインストール"
	@echo "  make wasm              WASM ビルド (要: source ~/.ghc-wasm/env)"
	@echo "  make serve             開発サーバー起動 (http://localhost:8080)"
	@echo "  make clean             ビルド成果物を削除"
