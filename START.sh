#!/usr/bin/env bash
mkdir ~/repos

cd ~/.home
rm -rf ~/.bash* ~/.git ~/.tmux* ~/.vim
stow zsh doom git tmux vim
xargs brew install < homebrewPackageList.txt

pip3 install black pyflakes isort pipenv nose pytest

read -p "Install XCode then press <RET> to continue set the defaults you like"

defaults write com.apple.dt.Xcode ShowBuildOperationDuration YES
