#!/bin/bash
# Backward-compatible wrapper — delegates to nbs.sh refresh.
# Translates old flag names: --playoff-date → --playoffs-from, --drop-date → --through
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

newargs=()
while [[ "$#" -gt 0 ]]; do
  case "$1" in
    --playoff-date) newargs+=(--playoffs-from "$2"); shift 2 ;;
    --drop-date)    newargs+=(--through "$2");       shift 2 ;;
    *)              newargs+=("$1");                  shift ;;
  esac
done

exec bash "$SCRIPT_DIR/nbs.sh" refresh "${newargs[@]}"
