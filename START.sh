cd ~
git init
git remote add origin git@github.com:LucianoLaratelli/-HOME.git
git fetch
git checkout -f master
git submodule init
git submodule update
yes | doom install
