#!/usr/bin/env bash
mkdir ~/repos

sudo pacman -S - <pkglist.txt
pip install --user -r pippkg.txt
pushd ~/repos

git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
popd

#prevent dropbox automatic updates
install -dm0 ~/.dropbox-dist

#prevent keyserver import failure when using yay to get dropbox:
#https://bbs.archlinux.org/viewtopic.php?pid=1917184#p1917184
cp /etc/pacman.d/gnupg/gpg.conf ~/.gnupg/gpg.conf
echo "keyserver pool.sks-keyservers.net" >>~/.gnupg/gpg.conf

yay dropbox
yay bear
yay snapd
yay franz

echo "********************************************************************************"
echo "*****************************start dropbox please*******************************"
echo "********************************************************************************"
read -p "Press Enter to continue"

sudo systemctl edit dropbox@{USER}
sudo systemctl enable dropbox@{USER}

sudo systemctl enable --now snapd.socket

cd ~/.home
rm -rf ~/.bash* ~/.git ~/.tmux* ~/.vim
stow bash doom git tmux vim

cd ~/repos

git clone git@github.com:rupa/z.git

git clone git@github.com:emacs-mirror/emacs.git
cd emacs
git checkout -t origin/emacs-27

./autogen.sh
CFLAGS="-O2 -march=native" ./configure --with-modules --quiet
NUMCORES="$(grep -c ^processor /proc/cpuinfo)"
NUMCORES=$(($NUMCORES * 2))
make -j${NUMCORES}
sudo make install

cd
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install
~/.emacs.d/bin/doom compile

#install icons
cd
mkdir .icons
pushd .icons
git clone https://github.com/rtlewis88/rtl88-Themes.git
cd rtl88-Themes
git checkout Arc-ICONS
popd
cp -r rtl88-Themes/Arc-ICONS .
rm -rf rtl88-Themes

#install themes
cd
mkdir .themes
pushd .themes
git clone https://github.com/rtlewis88/rtl88-Themes.git
cd rtl88-Themes
git checkout Arc-Darkest-COLORS-Complete-Desktop
popd
cp -r rtl88-Themes/AD-Plum .
rm -rf rtl88-Themes

#rbenv install
cd
git clone https://github.com/rbenv/rbenv.git ~/.rbenv

git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build

${HOME}/.rbenv/bin/rbenv install 2.7.1
${HOME}/.rbenv/bin/rbenvv global 2.7.1
ruby -v

gem install bundler
rbenv rehash

pip install black pyflakes isort pipenv nose pytest

#install rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add rls

#firefox customization
pushd ~/.mozilla/firefox/*default-release
mkdir chrome
cp ~/.home/.userChrome.css userChrome.css
popd

pushd ~/Dropbox/stow
rm ~/.bash_history && stow --target=${HOME} bash
popd

echo "************************************************************************"
echo "*****************************restarting now*****************************"
echo "************************************************************************"
read -p "Press Enter to continue"
sudo shutdown -r now
