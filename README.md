# What is this?

My .emacs.d with git subtrees instead of melpa or whatever.

https://spwhitton.name/blog/entry/emacs-pkg-subtree/

Also based on technomancy's emacs-starter-kit

https://github.com/technomancy/emacs-starter-kit

# Requirements

Works with emacs 26.3

# Installation

Clone this repo to replace as your ~/.emacs.d and launch emacs, it will take a few minutes to compile.

## Magit setup

Create a file ~/.emacs.d/pkg/magit/config.mk with the contents:

    LOAD_PATH  = -L ~/.emacs.d/pkg/magit/lisp
    LOAD_PATH += -L ~/.emacs.d/pkg/dash.el
    LOAD_PATH += -L ~/.emacs.d/pkg/hydra
    LOAD_PATH += -L ~/.emacs.d/pkg/transient
    LOAD_PATH += -L ~/.emacs.d/pkg/with-editor
    LOAD_PATH += -L ~/.emacs.d/pkg/magit-popup
    LOAD_PATH += -L ~/.emacs.d/pkg/ghub

Then run `make` inside that same directory

## Haskell setup

Probably install haskell with nix

    cd ~/.emacs.d/pkg/haskell-mode
    make haskell-mode-autoloads.el

## Python

Install [pyenv](https://github.com/pyenv/pyenv) probably with [pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv)

And _before_ you open a python file do `M-x pyenv-mode-set` and choose your
pyenv env. Also do `M-x pythonic-activate` and navigate to your virtualenv for
anaconda to work well.

## Rust

Works with rust-src

# Usage

Read the post in the first link and use that method to add or remove packages.

If you add new packages you might want to recompile with `(reinit-pkgs)` in ~/.emacs.d/init.el. It takes a few minutes to recompile so you might wanna add your packages in bulk before recompiling.

It's sometimes tricky to add "packages" because you have to add their dependencies manually and also make sure that the versions are actually compatible with each other. But that annoyance might be worthwhile if it lets you avoid melpa.
