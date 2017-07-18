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

## Specifying preferred applications

### Using dired-launch-extensions-map

One can use `dired-launch-extensions-map` to specify, for a given file extension, one or more preferred applications by simply specifying the application as a string.

```
(setf dired-launch-extensions-map
      '(;; specify LibreOffice as the preferred application for
        ;; a Microsoft Excel file with the xslx extension
        ("xlsx" ("libreofficedev5.3"))
        ;; specify LibreOffice and Abiword as preferred applications for
        ;; an OpenDocument text file with the odt extension
        ("odt" ("libreofficedev5.3" "abiword"))))
```

One can also use `dired-launch-extensions-map` to specify, for a given file extension, an arbitrary function to invoke.

This entry specifies, for files with the 'html' extension, calling the 'bluefish' executable with the '-n' option.

```
(list "html"
      (list (list "special html launcher"
		  #'(lambda (file)
		      (message "encountered an HTML file: %s" file)
		      ;; invoke arbitrary command
		      (dired-launch-call-process-on "bluefish" "-n" file)))))
```

Here, the second member of `dired-launch-extensions-map` specifies, for files with the 'txt' extension, Emacs should directly open the application using `find-file`.

```
(setf dired-launch-extensions-map 
      (list
       ;; LibreOffice and Abiword as preferred applications for
       ;; an OpenDocument text (odt) file
       '("odt" ("libreofficedev5.3" "abiword"))
       ;; open text files with Emacs
       '("txt" (("emacs" find-file)))))
```

### Linux

Change the preferred (default) application for a given file type by invoking mimeopen with the '-d' flag:

       mimeopen -d myfile

## Related projects

[Launch](https://github.com/sfllaw/emacs-launch)

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

If you did not receive a copy of the GNU General Public License along with this program, see http://www.gnu.org/licenses/.
