;;; dired-launch.el --- Use dired as a launcher

;; Copyright (C) 2016 David Thompson
;; Author: David Thompson
;; Version: 0.2
;; Keywords: dired, launch
;; URL: https://github.com/thomp/dired-launch

;;; Commentary:

;; This package provides a launcher for the Emacs dired-mode.
;; In a nutshell, it lets you select a file and then launch an
;; external application with that file.

;; To enable, add `dired-launch-mode' to `dired-mode-hook'.  For
;; convenience, the command `dired-launch-enable' will do this for
;; you.

;;; Code:

(defvar dired-launch-default-launcher
  '("mimeopen" "-n")
  "Define the program used as the default launcher. The second member of the list defines a command-line flag which will be used when invoking the program.")

(defvar dired-launch-extensions-map
  (list '("odt" ("libreofficedev5.3" "abiword"))
	'("JPG" ("phototonic" "gimp"))
	'("png" ("phototonic"))
	(list "html"
	      (list (list "special html launcher"
			  (list #'(lambda (file)
				    (message "encountered an HTML file: %s" file)
				    ;; invoke arbitrary command
				    (dired-launch-call-process-on "bluefish" "-n" file))))
		    (list "travel back in time" "xedit")))
	(list "txt"
	      (list (list "emacs"
			  (list #'(lambda (file)
				    (find-file file)))))))
  "Defines preferred executable(s) for specified file extensions via an alist. Extensions are matched in a case-sensitive manner. The second member of each alist member is a list where each member is either a string corresponding to an executable or a list where the first member is a descriptive string and the second member is either a string or a funcallable object which accepts a single argument, a string corresponding to the file, and returns a string (which, presumably, represents an executable or something to invoke).")

(defvar dired-launch-completions-f
  #'(lambda (file)
      (let ((internal-completions (dired-launch--executables-list-using-user-extensions-map file)))
	(if internal-completions
	    (list internal-completions :user-extensions-map)
	  (list (dired-launch--executables-list file) :external))))
  "Specifies a function which should accept a single argument, a string corresponding to the file under consideration. The function should return two values, a set of completions and an indication of the source of the completions (either :user-extensions-map or :external). The first value returned, a set of completions (presumably corresponding to executables), is either a list of strings or an alist.")

(defun dired-launch-ditch-preferred-handler ()
  "Remove preferred handler for file(s) specified by dired-launch."
  (interactive)
  (let ((extensions nil)
	(files (dired-get-marked-files t current-prefix-arg)))
    (map nil #'(lambda (file)
	     (let ((extension (file-name-extension file)))
	       (unless (member extension extensions)
		 (push extension extensions)
		 (dired-launch-extensions-map-pop extension))))
	 files)))

(defun dired-launch-extensions-map-pop (extension)
  (pop (second (assoc extension dired-launch-extensions-map))))

(defun dired-launch-extensions-map-add-handler (extension handler)
  ;; add a member for the extension if such an entry does not exist
  (if (not (assoc extension dired-launch-extensions-map))
      (push (list extension (list handler))
	    dired-launch-extensions-map)
    (push handler (second (assoc extension dired-launch-extensions-map)))))

(defun dired-launch-homebrew (files launch-cmd)
  (mapc #'(lambda (file)
	    (let ((buffer-name "dired-launch-output-buffer")
		  (preferred-launch-cmd-spec
		   (let ((completions (dired-launch--executables-list-using-user-extensions-map file)))
		     (car completions))))
	      (cond ((stringp preferred-launch-cmd-spec)
		      (save-window-excursion
			(dired-launch-call-process-on preferred-launch-cmd-spec file)))
		    (preferred-launch-cmd-spec
		     (funcall (cadr preferred-launch-cmd-spec) file))
		    (t
		     (save-window-excursion
		       (let ((args (append (rest dired-launch-default-launcher)
					   (list file))))
			 (apply #'dired-launch-call-process-on
				launch-cmd
				args)))))))
	files))

(defun dired-launch-call-process-on (launch-cmd &rest args)
  (if (executable-find launch-cmd)
      ;; handle file names with spaces
      (apply #'call-process
	     (append (list launch-cmd
			   nil		; infile
			   0		; async-ish...
			   nil		; display
			   )
		     args))
    (message "Could not find %s. Is %s installed? Check the value of dired-launch-default-launcher." launch-cmd launch-cmd)))

;;;###autoload
(defun dired-launch-command ()
  "Attempt to launch appropriate executables on marked files in the current dired buffer."
  (interactive) 
  (cond ((eq system-type 'darwin)
	 (dired-launch-homebrew
	  (dired-get-marked-files t current-prefix-arg)
	  "open"))
	((eq system-type 'gnu/linux)
	 (dired-launch-homebrew
	  (dired-get-marked-files t current-prefix-arg)
	  (car dired-launch-default-launcher)))
	((eq system-type 'windows-nt) (dired-map-over-marks
				       (w32-shell-execute "open" (dired-get-filename) nil 1) 
				       nil))
	(t (error "%s is not supported" system-type))))

;;;###autoload
(defun dired-launch-with-prompt-command ()
  "For each marked file in the current dired buffer, prompt user to specify an executable and then call the specified executable using that file."
  (interactive)
  (if (eq system-type 'windows) 
      (message "Windows not supported")
    (mapc #'(lambda (marked-file)
	      (let ((launch-cmd-spec (dired-launch-get-exec--completions marked-file)))
		(if (stringp launch-cmd-spec)
		    (save-window-excursion
		      (dired-launch-call-process-on launch-cmd-spec marked-file))
		  (funcall launch-cmd-spec marked-file))))
	  (dired-get-marked-files t current-prefix-arg))))

(defun dired-launch-get-exec--simple ()
  (read-from-minibuffer (concat "Launch " file " with? ")))

(defun dired-launch-get-exec--completions (file)
  "Prompt user to select a completion. Return the corresponding value (either the completion value itself or, if completions are specified as an alist, the value corresponding to the alist key."
  (let ((completions-and-source (funcall dired-launch-completions-f file)))
    (let ((completions (car completions-and-source)))
     (let ((selection (minibuffer-with-setup-hook 'minibuffer-complete
			(completing-read (concat "Executable to use: ")
					 completions))))
       ;; if internal preferred handler isn't defined, offer to "remember" (short-term memory... no session persistence) selection 
       (if (not (eq (second completions-and-source) :user-extensions-map))
	   ;; ultimately, desirable to offer persistence and not just short-term memory
	   (let ((extension (file-name-extension file)))
	     (let ((rememberp (y-or-n-p (format "Use %s as preferred handler for %s files?" selection extension))))
	       (if rememberp
		   (if extension
		       (dired-launch-extensions-map-add-handler extension selection))))))
       ;; COMPLETIONS is either a list of strings or an alist
       (cond ((stringp (car completions))
	      selection)
	     ((consp (first completions))
	      (cadr (assoc selection completions)))
	     (t
	      (error "%s" "Can't handle COMPLETIONS")))))))

;; purloined from lisp/shell.el's 'shell--command-completion-data'
(defun dired-launch--executables-list (&optional file)
  (let ((path-dirs (append (cdr (reverse exec-path))
			   (if (memq system-type '(windows-nt ms-dos)) '("."))))
	(cwd (file-name-as-directory (expand-file-name default-directory)))
	(ignored-extensions
	 (and comint-completion-fignore
	      (mapconcat (function (lambda (x) (concat (regexp-quote x) "\\'")))
			 comint-completion-fignore "\\|")))
	(completions ()))
    ;; Go thru each dir in the search path, finding completions.
    (while path-dirs
      (setq dir (file-name-as-directory (comint-directory (or (car path-dirs) ".")))
	    comps-in-dir (and (file-accessible-directory-p dir)
			      (file-name-all-completions "" dir)))
      ;; Go thru each completion found, to see whether it should be used.
      (while comps-in-dir
	(setq file (car comps-in-dir)
	      abs-file-name (concat dir file))
	(if (and (not (member file completions))
		 (not (and ignored-extensions
			   (string-match ignored-extensions file)))
		 (or (string-equal dir cwd)
		     (not (file-directory-p abs-file-name)))
		 (or nil ;(null shell-completion-execonly)
		     (file-executable-p abs-file-name)))
	    (setq completions (cons file completions)))
	(setq comps-in-dir (cdr comps-in-dir)))
      (setq path-dirs (cdr path-dirs)))
    completions))

(defun dired-launch--executables-list-using-mailcap (file)
  (mailcap-file-default-commands (list file)))

(defun dired-launch--executables-list-using-user-extensions-map (file)
  (let* ((extension (file-name-extension file nil))
	 (match (assoc extension dired-launch-extensions-map)))
    (cadr match)))


(defvar dired-launch-mode-map (make-sparse-keymap)
  "Keymap for `dired-launch-mode'.")

(define-key dired-launch-mode-map (kbd "J") 'dired-launch-command)
(define-key dired-launch-mode-map (kbd "K") 'dired-launch-with-prompt-command)

;;;###autoload
(define-minor-mode dired-launch-mode
  "Add commands to launch executables."
  :lighter " Launch")

;;;###autoload
(defun dired-launch-enable ()
  "Ensure that `dired-launch-mode' will be enabled in `dired-mode'."
  (interactive)
  (add-hook 'dired-mode-hook 'dired-launch-mode))

;; either inactivate dired-launch completely or deal with keybindings
;; likely to interfere with use of wdired
(add-hook 'wdired-mode-hook #'(lambda () (dired-launch-mode -1)))

(provide 'dired-launch)
;;; dired-launch.el ends here
