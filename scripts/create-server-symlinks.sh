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

    # If destination file already exist, make a backup
    if [[ -f "$f_dst" ]] || [[ -d "$f_dst" ]]
    then
        echo "File '$f_dst' already exists -- creating backup at $f_dst.backup"
        mv $f_dst $f_dst.backup
    fi

    # Perform symlink
    echo "Symlinking $f_src and $f_dst"
    echo "ln -s $src_dir/$f $dst_dir/$f"
    ln -s $src_dir/$f $dst_dir/$f
done
