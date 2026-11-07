#!/bin/bash -eu

# An explicit destination also allows validation without touching Windows files.
SOURCE_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
if [ "$#" -gt 0 ]; then
  TARGET=$1
else
  if ! grep -qi microsoft /proc/sys/kernel/osrelease 2>/dev/null; then
    exit 0
  fi
  WINDOWS_HOME=$(powershell.exe -NoProfile -NonInteractive -Command 'Write-Output $env:USERPROFILE' | tr -d '\r')
  if [ -z "$WINDOWS_HOME" ]; then
    echo 'Unable to determine Windows USERPROFILE.' >&2
    exit 1
  fi
  TARGET="$(wslpath -u "$WINDOWS_HOME")/.wslconfig"
fi

if [ ! -e "$TARGET" ]; then
  cp "$SOURCE_DIR/.wslconfig" "$TARGET"
else
  TEMP=$(mktemp)
  trap 'rm -f "$TEMP"' EXIT
  # Preserve unrelated sections and settings, replacing only this managed key.
  awk '
    NR == FNR {
      if ($0 ~ /^networkingMode=/) setting = $0
      next
    }
    { sub(/\r$/, "") }
    /^[[:space:]]*\[/ {
      if (in_wsl2 && !written) {
        print setting
        written = 1
      }
      in_wsl2 = ($0 ~ /^[[:space:]]*\[wsl2\][[:space:]]*([#;].*)?$/)
      if (in_wsl2) found = 1
    }
    in_wsl2 && /^[[:space:]]*networkingMode[[:space:]]*=/ {
      if (!written) print setting
      written = 1
      next
    }
    { print }
    END {
      if (!found) print "\n[wsl2]"
      if (!written) print setting
    }
  ' "$SOURCE_DIR/.wslconfig" "$TARGET" > "$TEMP"
  if ! cmp -s "$TEMP" "$TARGET"; then
    BACKUP=$(mktemp "${TARGET}.backup.XXXXXX")
    cp -p "$TARGET" "$BACKUP"
    cp "$TEMP" "$TARGET"
    echo "Previous WSL configuration saved to $BACKUP"
  fi
fi

echo "WSL mirrored networking configured in $TARGET"
echo 'Save your work, run wsl --shutdown in Windows PowerShell, then reopen WSL to apply.'
