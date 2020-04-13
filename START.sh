pushd 
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install
cd repos
git submodule init
git submodule update
popd
