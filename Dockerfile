FROM quay.io/nabinno/dove-ubuntu16
USER action

WORKDIR /home/action
RUN sh -c "$(curl -fsSL https://raw.github.com/nabinno/dotfiles/master/install.sh)"
RUN eval "$(exec -l zshrc)"
