[![MELPA](https://melpa.org/packages/dired-launch-badge.svg)](https://melpa.org/#/dired-launch)

# dired-launch

*Launch an external application from dired*

---

**dired-launch** is a launcher for the Emacs [dired-mode](https://www.emacswiki.org/emacs/DiredMode). In a nutshell, it lets you select a file and then launch an external application with that file.

## Getting started

1. Download `dired-launch.el`.

2. Load `dired-launch.el`. For example, you might add the following line to `~/.emacs`:

    `(load "/path/to/dired-launch.el")`

3. Ensure `dired-launch` is enabled in `dired-mode`:

    `(dired-launch-enable)`


## Use

1. <kbd>M-x</kbd> `dired`

2. Move the cursor to a file of interest

3. Launch 

    <kbd>J</kbd> (`dired-launch-command`) launches the file using the preferred application 

	<kbd>K</kbd> (`dired-launch-with-prompt-command`) prompts for the application and then launches the file
	

## Modifying the default keybindings

If you prefer different key bindings, consider modifying the defaults.

    `(define-key dired-launch-mode-map (kbd "l") 'dired-launch-command)`


## Changing the preferred application

### Linux

Change the preferred (default) application for a given file type by invoking mimeopen with the '-d' flag:

       mimeopen -d myfile

## Related projects

[Launch](https://github.com/sfllaw/emacs-launch)
