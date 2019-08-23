#  export GDK_BACKEND=wayland 
   export CLUTTER_BACKEND=wayland
   export XDG_SESSION_TYPE=wayland
   export QT_WAYLAND_FORCE_DPI=physical
   export  QT_WAYLAND_DISABLE_WINDOWDECORATION=1
   export SDL_VIDEODRIVER=wayland
   export _JAVA_AWT_WM_NONREPARENTING=1
#  force firefox to use wayland 
   export MOZ_ENABLE_WAYLAND=1
  
  if [ -n "${commands[fzf-share]}" ]; then
     source "$(fzf-share)/key-bindings.zsh"
  fi

  if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
     XKB_DEFAULT_LAYOUT=it exec sway
  fi
