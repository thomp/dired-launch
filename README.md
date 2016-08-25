# dired-launch

**dired-launch** is a launcher for the emacs [dired-mode](https://www.emacswiki.org/emacs/DiredMode). In a nutshell, it lets you select one or more files and then launch an external application with that file.

## Getting started

1. Download `dired-launch.el`.

2. Ensure emacs loads `dired-launch.el`. For example, you might add the following line to your `~/.emacs`:

   `(load "/path/to/dired-launch.el")`

## Use

1. <kbd>M-x</kbd> `dired`

2. Move the cursor to a file of interest

3. Launch:

<kbd>l</kbd> launches the file using the default application

<kbd>L</kbd> prompts for the launch application and then launches the file

## Miscellany

Change the preferred (default) application for a given file type by invoking mimeopen with the '-d' flag:

       mimeopen -d myfile
