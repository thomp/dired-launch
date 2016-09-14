[![MELPA](https://melpa.org/packages/dired-launch-badge.svg)](https://melpa.org/#/dired-launch)

# dired-launch

*Launch an external application from dired*

---

**dired-launch** is a launcher for the Emacs [dired-mode](https://www.emacswiki.org/emacs/DiredMode). In a nutshell, it lets you select a file and then launch an external application with that file.

## Getting started

1. Download `dired-launch.el`.

2. Ensure emacs loads `dired-launch.el`. For example, you might add the following line to your `~/.emacs`:

    `(load "/path/to/dired-launch.el")`

3. Enable the dired minor mode:

    `(dired-launch-enable)`

4. If you prefer different key bindings, consider modifying them:

    `(define-key dired-launch-mode-map (kbd "l") 'dired-launch-command)`

## Use

1. <kbd>M-x</kbd> `dired`

2. Move the cursor to a file of interest

3. Launch:

    <kbd>J</kbd> launches the file using the default application using `dired-launch-command`


## Miscellany

Change the preferred (default) application for a given file type by invoking mimeopen with the '-d' flag:

       mimeopen -d myfile

## Related projects

[Launch](https://github.com/sfllaw/emacs-launch)
