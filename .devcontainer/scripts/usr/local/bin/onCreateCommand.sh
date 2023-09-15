#!/usr/bin/env bash
# Copyright (c) 2023 b-data GmbH.
# Distributed under the terms of the MIT License.

set -e

mkdir -p "$HOME/.cabal/bin"
mkdir -p "$HOME/.local/bin"

# Copy Zsh-related files and folders from the untouched home directory
if [ "$(id -un)" == "root" ]; then
  if [ ! -d /root/.oh-my-zsh ]; then
    cp -R /home/*/.oh-my-zsh /root;
  fi
  if [ ! -f /root/.zshrc ]; then
    cp /home/*/.zshrc /root;
  fi
else
  if [ ! -d "$HOME/.oh-my-zsh" ]; then
    sudo cp -R /root/.oh-my-zsh "$HOME";
    sudo chown -R "$(id -u)":"$(id -g)" "$HOME/.oh-my-zsh";
  fi
  if [ ! -f "$HOME/.zshrc" ]; then
    sudo cp /root/.zshrc "$HOME";
    sudo chown "$(id -u)":"$(id -g)" "$HOME/.zshrc";
  fi
fi

# Set PATH so it includes user's private bin if it exists
if ! grep -q "user's private bin" "$HOME/.zshrc"; then
  echo -e "\n# set PATH so it includes user's private bin if it exists\nif [ -d \"\$HOME/bin\" ] && [[ \"\$PATH\" != *\"\$HOME/bin\"* ]] ; then\n    PATH=\"\$HOME/bin:\$PATH\"\nfi" >> "$HOME/.zshrc";
  echo -e "\n# set PATH so it includes user's private bin if it exists\nif [ -d \"\$HOME/.local/bin\" ] && [[ \"\$PATH\" != *\"\$HOME/.local/bin\"* ]] ; then\n    PATH=\"\$HOME/.local/bin:\$PATH\"\nfi" >> "$HOME/.zshrc";
fi

# Set PATH so it includes cabal's bin if it exists
if ! grep -q "cabal's bin" "$HOME/.zshrc"; then
  echo -e "\n# set PATH so it includes cabal's bin if it exists\nif [ -d \"\$HOME/.cabal/bin\" ] && [[ \"\$PATH\" != *\"\$HOME/.cabal/bin\"* ]] ; then\n    PATH=\"\$HOME/.cabal/bin:\$PATH\"\nfi" >> "$HOME/.zshrc";
fi

# Enable Oh My Zsh plugins
sed -i "s/plugins=(git)/plugins=(cabal git pip screen stack tmux vscode)/g" "$HOME/.zshrc"

# Remove old .zcompdump files
rm -f "$HOME"/.zcompdump*
