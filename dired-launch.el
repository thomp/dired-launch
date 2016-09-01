;;; dired-launch.el --- Use dired as a launcher

;; Copyright (C) 2016 David Thompson
;; Author: David Thompson
;; Version: 0.1
;; Keywords: dired, launch
;; URL: https://github.com/thomp/dired-launch

;;; Commentary:

;; This package provides a launcher for the Emacs dired-mode.
;; In a nutshell, it lets you select a file and then launch an
;; external application with that file.

;;; Code:

;; DL-MAILCAP-FRIEND defines program and associated argument(s)
(defvar dired-launch-mailcap-friend
  '("mimeopen" "-n"))

(defun dired-launch-homebrew (files)
  (let ((launch-cmd (case system-type
		      (gnu/linux (first dired-launch-mailcap-friend))
		      (darwin "open"))))
    (mapc #'(lambda (file)
	      (let ((buffer-name "dired-launch-output-buffer"))
		(dired-launch-call-process-on launch-cmd file)))
	  files)))

(defun dired-launch-call-process-on (launch-cmd file)
  ;; handle file names with spaces
  (call-process launch-cmd
		nil	; infile
		0 ; async-ish...
		nil 
		(second dired-launch-mailcap-friend) file))

;;;###autoload
(defun dired-launch-command ()
  "Attempt to launch appropriate executables on marked files in the current dired buffer."
  (interactive) 
  (if (eq system-type 'windows)
      (dired-map-over-marks (shell-command (concat  "open " (dired-get-filename))
					   nil nil)
			    nil)
    (save-window-excursion
      (dired-launch-homebrew
       (dired-get-marked-files t current-prefix-arg)))))

;;;###autoload
(defun dired-launch-with-prompt-command ()
  "For each marked file in the current dired buffer, prompt user to specify an executable and then call the specified executable using that file."
  (interactive) 
  (if (eq system-type 'windows) 
      (message "Windows not supported")
    (save-window-excursion
      (mapc #'(lambda (file)
		(let ((launch-cmd (read-from-minibuffer (concat "Launch " file " with?" ))))
		  (dired-launch-call-process-on launch-cmd file))) 
	    (dired-get-marked-files t current-prefix-arg)))))

(defun dired-launch-default-key-bindings ()
  (define-key dired-mode-map (kbd "C-c l") 'dired-launch-command)
  (define-key dired-mode-map (kbd "C-c L") 'dired-launch-with-prompt-command))

(add-hook 'dired-load-hook (lambda (&rest ignore) (dired-launch-default-key-bindings)))

;; anticipate possibility that dired-load-hook (as defined above) was not invoked (e.g., dired loaded already)
(when (boundp 'dired-mode-map)
  (dired-launch-default-key-bindings))

(provide 'dired-launch)
;;; dired-launch.el ends here
