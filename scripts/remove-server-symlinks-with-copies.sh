#!/usr/bin/env sh

src_dir=/storage-01/$USER
dst_dir=$HOME

# All files that should be linked from src_dir to the home
# files=( dotfiles .zgen .zsh_history projects results .netrc .bash_history .tmux )
files=( dotfiles .zgen .zsh_history .netrc .bash_history .tmux )

for f in "${files[@]}"
do
    f_src="$src_dir/$f"
    f_dst="$dst_dir/$f"

    if [[ ! -f "$f_src" ]] && [[ ! -d "$f_src" ]]
    then
        echo "Source file $f_src does not exist! Skipping ..."
        continue
    fi

    # Perform symlink
    echo "Removing symlinking from $f_dst to $f_src"
    echo "rm -f $f_dst"
    rm -f $f_dst

    echo "Copying from $f_src to $f_dst"
    cp -r $f_src $f_dst
done
