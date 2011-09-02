;;
;; dired-launch.el: a dired tweak -> use dired as a launcher
;;
;; - default: launch files with 'l' key
;;

;;
;; - see also:
;;     http://omniorthogonal.blogspot.com/2008/05/useful-emacs-dired-launch-hack.html
;;     http://www.emacswiki.org/emacs/AnythingLauncher
;;     ORG-OPEN-AT-POINT
;;

;; using this is problematic since only one process can run at a time (issue: inability to specify distinct async output buffers for each process...)
(defun dired-launch-builtin
  ;; dired-do-async-shell-command has issues... but plays nicely with xdg-open
  ;; dired-do-shell-command is a drag since emacs locks until launched app terminates
  (dired-do-async-shell-command
   (case system-type
     (gnu/linux "xdg-open") ; kde-open, gnome-open, ...
     (darwin "open"))
   nil
   (dired-get-marked-files t current-prefix-arg)))

;; workaround for DIRED-DO-ASYNC-SHELL-COMMAND (see DIRED-LAUNCH-BUILTIN) inability to handle more than one process at a time (due to inability to specify distinct async buffer names?)
(defun dired-launch-homebrew ()
  (let ((launch-cmd 	   
	 (case system-type
	   (gnu/linux "xdg-open")	; kde-open, gnome-open, ...
	   (darwin "open")))
	(files (dired-get-marked-files t current-prefix-arg)))
    (mapc #'(lambda (file)
		 (let ((cmd 
			(concat
			 launch-cmd
			 " "
			 (shell-quote-argument file))))
		   (start-process-shell-command 
		    cmd
		    nil			  ; don't associate process w/a buffer
		    cmd)))
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
