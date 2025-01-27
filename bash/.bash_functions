#!/bin/bash
#
# Define some functions that are loaded in bash but can also be loaded in zsh/fish
#


function eog () {
  command eog $1 & disown
}

function emacs () {
  command emacs $1 & disown
}

function evince () {
  command evince $1 & disown
}

function pdf () {
  command evince $1 & disown
}

function loupe () {
  command loupe $1 & disown
}

function maybe_activate_conda_env () {

  # Check if "conda" command is available,
  if ! command -v conda &>/dev/null; then
    return
  fi

  # Check if conda env is set
  if [ ! -z "${CONDA_DEFAULT_ENV}" ]; then
    dirname=${PWD##*/}  # Get directory name without full path

    # Check if conda env is part of current pwd (allows for PWD being a subdir)
    if [[ "${PWD}" == *"${CONDA_DEFAULT_ENV}"* ]]; then
      return
    else
      echo "Deactivating conda environment ${CONDA_DEFAULT_ENV}"
      conda deactivate
      return
    fi
  fi


  # Get directory name without full path
  dirname=${PWD##*/}
  # If directory name can be found in conda evironments, activate it!
  if grep -q $dirname <(command ls ~/.conda/envs/); then
    echo "Conda environment '$dirname' found! Activating now ..."
    conda activate $dirname
  fi
}
