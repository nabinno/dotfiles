#!/bin/bash -eu

# An explicit destination also allows validation without touching Windows files.
SOURCE_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
PROFILE_SOURCE="$SOURCE_DIR/MyDocuments/WindowsPowerShell/Microsoft.PowerShell_profile.ps1"

if [ "$#" -gt 0 ]; then
  WINDOWS_HOME=$1
else
  if ! grep -qi microsoft /proc/sys/kernel/osrelease 2>/dev/null; then
    exit 0
  fi
  WINDOWS_USERPROFILE=$(powershell.exe -NoProfile -NonInteractive -Command 'Write-Output $env:USERPROFILE' | tr -d '\r')
  if [ -z "$WINDOWS_USERPROFILE" ]; then
    echo 'Unable to determine Windows USERPROFILE.' >&2
    exit 1
  fi
  WINDOWS_HOME=$(wslpath -u "$WINDOWS_USERPROFILE")
fi

install_profile() {
  local target=$1
  mkdir -p "$(dirname "$target")"
  if [ -e "$target" ] && ! cmp -s "$PROFILE_SOURCE" "$target"; then
    local backup
    backup=$(mktemp "${target}.backup.XXXXXX")
    cp -p "$target" "$backup"
    echo "Previous PowerShell profile saved to $backup"
  fi
  cp "$PROFILE_SOURCE" "$target"
  echo "PowerShell profile applied to $target"
}

# Windows PowerShell 5.1
install_profile "$WINDOWS_HOME/Documents/WindowsPowerShell/Microsoft.PowerShell_profile.ps1"
# PowerShell 7+ (pwsh)
install_profile "$WINDOWS_HOME/Documents/PowerShell/Microsoft.PowerShell_profile.ps1"
