# Add `~/bin` to the `$PATH`
export PATH="$HOME/bin:$PATH"
PATH=/usr/local/bin/:$PATH
PATH="${HOME}/.emacs.d/bin/":$PATH

fortune | cowsay

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
for file in ~/.{path,bash_prompt,exports,aliases,functions}; do
	[ -r "$file" ] && [ -f "$file" ] && source "$file"
done
unset file

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Autocorrect typos in path names when using `cd`
shopt -s cdspell

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
	shopt -s "$option" 2>/dev/null
done

# Add tab completion for many Bash commands
if which brew &>/dev/null && [ -f "$(brew --prefix)/share/bash-completion/bash_completion" ]; then
	source "$(brew --prefix)/share/bash-completion/bash_completion"
elif [ -f /etc/bash_completion ]; then
	source /etc/bash_completion
fi

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh

complete -o "nospace" -W "Contacts Calendar Dock Finder Mail Safari iTunes SystemUIServer Terminal Twitter" killall

export GPG_TTY=$(tty)
source "$HOME/.cargo/env"
export PATH="/usr/local/sbin:$PATH"
eval "$(/opt/homebrew/bin/brew shellenv)"

export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK="/Users/luciano/.gnupg/S.gpg-agent.ssh"
gpgconf --launch gpg-agent
