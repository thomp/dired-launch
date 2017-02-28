[![MELPA](https://melpa.org/packages/dired-launch-badge.svg)](https://melpa.org/#/dired-launch)

# dired-launch

*Launch an external application from dired*

---

**dired-launch** is a launcher for the Emacs [dired-mode](https://www.emacswiki.org/emacs/DiredMode). In a nutshell, it lets you select a file and then launch an external application with that file.

## Getting started

If MELPA is [enabled](https://melpa.org/#/getting-started) as an Emacs package repository, it should be possible to install simply using <kbd>M-x</kbd> `list-packages` and selecting the `dired-launch` package for installation (<kbd>i</kbd> <kbd>x</kbd>).

### Getting started without the Emacs package manager

1. Download `dired-launch.el`.

2. Load `dired-launch.el`. For example, you might add the following line to `~/.emacs`:

    `(load "/path/to/dired-launch.el")`

3. Ensure `dired-launch` is enabled in `dired-mode`:

    `(dired-launch-enable)`

### Ensure dired-launch is configured as desired

Ensure the default launcher specified by `dired-launch-default-launcher` is the desired program. For example, one might specify `xdg-open` as the default launcher:

    ;; use xdg-open as the default launcher
	(setq dired-launch-default-launcher '("xdg-open"))

## Use

1. <kbd>M-x</kbd> `dired`

2. Move the cursor to a file of interest

3. Launch 

    <kbd>J</kbd> (`dired-launch-command`) launches the file using the preferred application 

	<kbd>K</kbd> (`dired-launch-with-prompt-command`) prompts for the application and then launches the file
	

## Modifying the default keybindings

If you prefer different key bindings, consider modifying the default keybinding(s).

```
;; bind the 'l' key to dired-launch-command
(define-key dired-launch-mode-map (kbd "l") 'dired-launch-command)
```

## Changing the preferred application

### Linux

Change the preferred (default) application for a given file type by invoking mimeopen with the '-d' flag:

       mimeopen -d myfile

## Related projects

[Launch](https://github.com/sfllaw/emacs-launch)
