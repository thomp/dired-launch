;;
;; dired-launch.el: a dired tweak -> use dired as a launcher
;;

;;
;; use:
;; - launch files with 'l' key
;; - change preferred application with 'mimeopen -a myfile'
;;


;; DL-MAILCAP-FRIEND defines program and associated argument(s)
(defvar dl-mailcap-friend
  ;; "exo-open"
  ;; GNOME-OPEN
  ;; KDE-OPEN
  '("mimeopen" "-n")
  ;; XDG-OPEN 
  ;; - issues w/filenames with spaces (see https://bugs.archlinux.org/task/19305, https://bugs.launchpad.net/xdg-utils/+bug/220750)
  )

;; using this is problematic since only one process can run at a time (issue: inability to specify distinct async output buffers for each process...)
(defun dired-launch-builtin
  ;; dired-do-async-shell-command has issues... but plays nicely with xdg-open
  ;; dired-do-shell-command is a drag since emacs locks until launched app terminates
  (dired-do-async-shell-command
   (case system-type
     (gnu/linux dl-mailcap-friend)
     (darwin "open"))
   nil
   (dired-get-marked-files t current-prefix-arg)))

(defun dired-launch-homebrew ()
  (let ((launch-cmd 	   
	 (case system-type
	   (gnu/linux (first dl-mailcap-friend))
	   (darwin "open")))
	(files (dired-get-marked-files t current-prefix-arg)))
    (mapc #'(lambda (file)
	      (let ((buffer-name "dired-launch-output-buffer")
		    (cmd 
		     (concat
		      launch-cmd
		      " "
		      (shell-quote-argument file))))
		(message cmd) 
		;; handle file names with spaces
		(call-process launch-cmd
			      nil	; infile
			      0 ; async-ish...
			      nil 
			      (second dl-mailcap-friend) file
			      )))
	  files)))

(defun dired-launch-command ()
  (interactive) 
  (if (eq system-type 'windows)
      (dired-map-over-marks
       (w32-shell-execute "open" (dired-get-filename) nil 1))
    ;; forego persistent *Async Shell Command* buffer
    (save-window-excursion
      (dired-launch-homebrew))))

(setq dired-load-hook
      (lambda (&rest ignore)
	(define-key dired-mode-map
	  "l" 'dired-launch-command)))
