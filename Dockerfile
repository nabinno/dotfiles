FROM quay.io/nabinno/dove-ubuntu12
USER action

WORKDIR /home/action
RUN git clone https://github.com/nabinno/dotfiles.git
RUN find ~/dotfiles -maxdepth 1 -mindepth 1 | xargs -i mv -f {} ~/
RUN rm -fr .git Dockerfile README.md
RUN zsh -c "yes | source ~/.zshrc"

EXPOSE 22
CMD sudo /usr/sbin/sshd -D
