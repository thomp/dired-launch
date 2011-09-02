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

(defvar dl-mailcap-friend
  ;; EXO-OPEN
  "exo-open"
  ;; GNOME-OPEN
  ;; KDE-OPEN
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
	   (gnu/linux dl-mailcap-friend)
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
		(call-process dl-mailcap-friend 
			      nil
			      0 ; async-ish...
			      nil file)))
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
