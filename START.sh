#!/usr/bin/env bash
pushd ~ || (
	echo "pushd failed to push ~ onto directory stack"
	exit
)
pushd ~/.home || (
	echo "pushd failed to push ~/.home onto directory stack"
	exit
)
stow bash doom git tmux vim
popd || (
	echo "popd failed to pop top of directory stack"
	exit
)
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install
doom compile
pushd ~/repos || (
	echo "pushd failed to push ~/repos onto directory stack"
	exit
)
git clone git@github.com:rupa/z.git
popd
