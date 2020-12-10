#!/usr/bin/env bash
USAGE="$0 light|dark"

TERMITE_CONF_FOLDER=~/.config/termite
THEME=$1

change_theme() {
    echo "# THIS FILE HAS BEEN AUTOGENERATED. DO NOT CHANGE MANUALLY!" | \
        cat - $TERMITE_CONF_FOLDER/config.base $TERMITE_CONF_FOLDER/themes/$1 > $TERMITE_CONF_FOLDER/config
    killall -USR1 termite || true
}

case $THEME in
    light) change_theme modus-operandi ;;
    dark) change_theme modus-vivendi ;;
    one-dark) change_theme one-dark ;;
    *) echo $USAGE ;;
esac
