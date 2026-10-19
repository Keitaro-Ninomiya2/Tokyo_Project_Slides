#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
# Explicitly expose biblatex's bbx/cbx subdirectories on minimal TeX installs.
biblatex_style=$(kpsewhich biblatex.sty)
if [[ -n "$biblatex_style" ]]; then
  export TEXINPUTS="$(dirname "$biblatex_style")//:${TEXINPUTS:-}"
fi
pdflatex -interaction=nonstopmode -halt-on-error main.tex
if rg -q '<bcf:citekey' main.bcf; then
  biber main
fi
pdflatex -interaction=nonstopmode -halt-on-error main.tex
