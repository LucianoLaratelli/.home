#!/usr/bin/env bash
mkdir ~/repos

if cat /etc/*release | grep "arch"; then
	cd ~/repos
	git clone https://aur.archlinux.org/yay.git
	cd yay
	makepkg -si
	cd
	yay git
	yay dropbox
fi

echo "********************************************************************************"
echo "*****************************start dropbox please*******************************"
echo "********************************************************************************"
read -p "Press Enter to continue"

cd ~/.home
stow bash doom git tmux vim
source ~/.bashrc

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

cd ~/Dropbox/stow
stow --target=${HOME} bash
