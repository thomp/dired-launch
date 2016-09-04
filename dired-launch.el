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

;; To enable, add `dired-launch-mode' to `dired-mode-hook'.  For
;; convenience, the command `dired-launch-enable' will do this for
;; you.

;;; Code:

;; DL-MAILCAP-FRIEND defines program and associated argument(s)
(defvar dired-launch-mailcap-friend
  '("mimeopen" "-n"))

(defun dired-launch-homebrew (files launch-cmd)
  (mapc #'(lambda (file)
	    (let ((buffer-name "dired-launch-output-buffer"))
	      (dired-launch-call-process-on launch-cmd file)))
	files))

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
  (cond ((eq system-type 'darwin)
	 (save-window-excursion
	   (dired-launch-homebrew
	    (dired-get-marked-files t current-prefix-arg)
	    "open")))
	((eq system-type 'gnu/linux)
	 (save-window-excursion
	   (dired-launch-homebrew
	    (dired-get-marked-files t current-prefix-arg)
	    (first dired-launch-mailcap-friend))))
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
    (save-window-excursion
      (mapc #'(lambda (file)
		(let ((launch-cmd (read-from-minibuffer (concat "Launch " file " with?" ))))
		  (dired-launch-call-process-on launch-cmd file))) 
	    (dired-get-marked-files t current-prefix-arg)))))

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

(provide 'dired-launch)
;;; dired-launch.el ends here
