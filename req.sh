#!/bin/bash
sudo apt-get update && sudo apt-get install -y xsel emacs-nox xclip fonts-ibm-plex feh mpv cmus vim firefox git curl i3 xinit xterm tmux openconnect arandr i3lock docker docker-compose
curl -sL https://deb.nodesource.com/setup_13.x | sudo -E bash -
curl -sL https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
     echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
     sudo apt-get update && sudo apt-get install -y nodejs yarn
sudo yarn global add eslint prettier

