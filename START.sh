#!/usr/bin/env bash
mkdir ~/repos

cd ~/.home
rm -rf ~/.bash* ~/.git ~/.tmux* ~/.vim
stow zsh doom git tmux vim
xargs brew install < homebrewPackageList.txt

pip3 install black pyflakes isort pipenv nose pytest

brew install
