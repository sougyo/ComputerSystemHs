# ComputerSystem Haskell + WASM ビルド
GHC      := ghc
SRC      := src/Main.hs
SRCS     := $(wildcard src/*.hs src/**/*.hs)
OUTDIR   := web
WASM_OUT := $(OUTDIR)/computersystem.wasm
NATIVE   := /tmp/cputest

# ── ネイティブビルド (開発・テスト用) ──────────
.PHONY: native
native: $(NATIVE)
$(NATIVE): $(SRCS)
	$(GHC) --make -isrc $(SRC) -o $(NATIVE) -O2
	@echo "✓ Native build OK: $(NATIVE)"

# ── ネイティブテスト実行 ─────────────────────
.PHONY: test
test: native
	$(NATIVE)

# ── WASM ビルド ──────────────────────────────
# 事前に GHC WASM バックエンドのインストールが必要:
#   curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/\
#        raw/master/bootstrap.sh | sh
#   source ~/.ghc-wasm/env
.PHONY: wasm
wasm: $(WASM_OUT)
$(WASM_OUT): $(SRCS)
	wasm32-wasi-ghc --make -isrc $(SRC) \
	  -o $(WASM_OUT) \
	  -optl-Wl,--export=hs_get_layout \
	  -optl-Wl,--export=hs_get_state \
	  -optl-Wl,--export=hs_get_wires \
	  -optl-Wl,--export=hs_step \
	  -optl-Wl,--export=hs_reset \
	  -optl-Wl,--export=hs_load_asm \
	  -optl-Wl,--export=malloc \
	  -optl-Wl,--export=free
	@echo "✓ WASM build OK: $(WASM_OUT)"

# cabal を使う場合
.PHONY: cabal-build
cabal-build:
	cabal build

.PHONY: cabal-run
cabal-run:
	cabal run computersystem

# ── 開発サーバー ──────────────────────────────
.PHONY: serve
serve:
	python3 -m http.server 8080 --directory $(OUTDIR)

# ── WASM のインストール方法 ───────────────────
.PHONY: install-wasm-ghc
install-wasm-ghc:
	@echo "GHC WASM バックエンドのインストール:"
	@echo "  curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/raw/master/bootstrap.sh | sh"
	@echo "  source ~/.ghc-wasm/env"

.PHONY: clean
clean:
	rm -f $(NATIVE) $(WASM_OUT) src/*.o src/*.hi src/**/*.o src/**/*.hi
