#!/usr/bin/env bash
cd ~/.home
stow bash doom git tmux vim
source ~/.bashrc

mkdir ~/repos
cd ~/repos

git clone git@github.com:rupa/z.git

git clone git@github.com:emacs-mirror/emacs.git
cd emacs
git checkout -t origin/emacs-26
./autogen.sh
CFLAGS="-O2" ./configure --with-modules --quiet
NUMCORES="$(grep -c ^processor /proc/cpuinfo)"
NUMCORES=$(($NUMCORES * 2))
make -j${NUMCORES}

cd
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install
doom compile
