#!/bin/bash -eu

# Ensure the script stops on error
# -e: Exit immediately if a command exits with a non-zero status.
# -u: Treat unset variables as an error when substituting.

# Define color codes for output
GREEN=$(tput setaf 2)
RESET=$(tput sgr0)

# Installation directory
INSTALL_DIR="$HOME/.local/dotfiles"

# Main installation logic
main() {
  # Clean up previous installation if it exists
  if [ -d "$INSTALL_DIR" ]; then
    echo "Removing existing dotfiles directory: $INSTALL_DIR"
    rm -rf "$INSTALL_DIR"
  fi

  # Create the installation directory
  echo "Creating installation directory: $INSTALL_DIR"
  mkdir -p "$INSTALL_DIR"

  # Clone the repository
  echo "Cloning dotfiles repository..."
  git clone https://github.com/nabinno/dotfiles.git "$INSTALL_DIR"

  # Copy files to home directory using rsync for more control
  echo "Copying dotfiles to home directory..."
  rsync -av \
    --exclude='.git' \
    --exclude='install.sh' \
    --exclude='README.md' \
    --exclude='LICENSE' \
    --exclude='dotfiles.jpg' \
    --exclude='go.mod' \
    --exclude='go.sum' \
    --exclude='Dockerfile' \
    "$INSTALL_DIR/" "$HOME/"

  # Display success message
  cat <<EOF
${GREEN}
       __      __  _____ __
  ____/ /___  / /_/ __(_) /__  _____
 / __  / __ \/ __/ /_/ / / _ \/ ___/
/ /_/ / /_/ / /_/ __/ / / __ (__)
\__,_/\____/\__/_/ /_/_/\___/____/
                                     ...is now installed!
${RESET}
EOF
}

# Execute the main function
main
