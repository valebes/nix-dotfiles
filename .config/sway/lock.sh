#!/bin/sh

grim /tmp/lock.png
SCREEN=/tmp/lock.png
ICON=~/.config/sway/lock.png
PARAM=(--font "mononoki Nerd Font Mono" --indicator-radius 100 --line-color 458588 --ring-color 83a598 --line-ver-color 458588 --ring-ver-color fe8019  --line-wrong-color cc241d --ring-wrong-color fb4934 --line-clear-color d79921 --ring-clear-color fabd2f )

BLURTYPE="2x8"

convert $SCREEN -blur $BLURTYPE $SCREEN
convert $SCREEN $ICON -composite $SCREEN

swaylock "${PARAM[@]}" -i "$SCREEN" > /dev/null 2>&1

