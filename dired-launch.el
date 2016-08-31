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
(defvar dl-mailcap-friend
  '("mimeopen" "-n"))

(defun dired-launch-homebrew (files)
  (let ((launch-cmd (case system-type
		      (gnu/linux (first dl-mailcap-friend))
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
		(second dl-mailcap-friend) file))

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

(setq dired-load-hook
      (lambda (&rest ignore)
	(define-key dired-mode-map "l" 'dired-launch-command)
	(define-key dired-mode-map "L" 'dired-launch-with-prompt-command)))

;; anticipate possibility that dired-load-hook (as defined above) was not invoked (e.g., dired loaded already)
(when (boundp 'dired-mode-map)
  (define-key dired-mode-map "l" 'dired-launch-command)
  (define-key dired-mode-map "L" 'dired-launch-with-prompt-command))

(provide 'dired-launch)
;;; dired-launch.el ends here
