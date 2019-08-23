#!/usr/bin/env zsh

zsh -c 'print -rl -- ${(ko)commands} | fzf | xargs -r swaymsg -t command exec'

