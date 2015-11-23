FROM quay.io/nabinno/dove-ubuntu12
USER action

WORKDIR /home/action
RUN sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install.sh)"
RUN zsh -c "yes | source ~/.zshrc"

EXPOSE 22
CMD sudo /usr/sbin/sshd -D
