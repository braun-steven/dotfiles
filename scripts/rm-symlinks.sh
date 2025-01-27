for f in $(stow -n -v -t ~/ * 2>&1 | grep existing | cut -d":" -f 2); do
    target="$HOME/$(echo $f | xargs)"
    if [ -L "$target" ]; then
        echo "$target yes"
        rm "$target"
    else
        echo "$target no"
    fi
done
