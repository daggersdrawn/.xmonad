# ~/.xmonad/autostart.sh
#!/bin/sh

# Programs which will run after Xmonad has started
if ! pgrep "urxvtcd"; then
  #urxvtcd -title term -q -f -o &
fi
if ! pgrep "emacs"; then
  #emacs -u rizumu --daemon --eval "(server-start)"
  #(sleep 2 && emacsclient --no-wait ~/ &)
fi
if ! pgrep "firefox"; then
  firefox &
fi
if ! pgrep "pidgin"; then
  pidgin &
fi
if ! pgrep "skype"; then
  skype &
fi
