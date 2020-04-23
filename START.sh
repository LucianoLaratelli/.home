pushd 
# cd .home
# stow 
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install
cd repos
git clone git@github.com:rupa/z.git
popd
