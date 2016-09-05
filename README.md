# dired-launch

*Launch an external application from dired*

---

**dired-launch** is a launcher for the Emacs [dired-mode](https://www.emacswiki.org/emacs/DiredMode). In a nutshell, it lets you select a file and then launch an external application with that file.

## Getting started

1. Download `dired-launch.el`.

2. Ensure emacs loads `dired-launch.el`. For example, you might add the following line to your `~/.emacs`:

   `(load "/path/to/dired-launch.el")`
   
3. Use <kbd>M-x dired-launch-enable</kbd> (or add `dired-launch-mode` to `dired-mode-hook`)

## Usage

1. <kbd>M-x</kbd> `dired`

2. Move the cursor to a file of interest

3. Launch:

<kbd>J</kbd> launches the file using the default application

<kbd>K</kbd> prompts for the launch application and then launches the file

## Miscellany

Change the preferred (default) application for a given file type by invoking mimeopen with the '-d' flag:

       mimeopen -d myfile

## Related projects

[Launch](https://github.com/sfllaw/emacs-launch)
