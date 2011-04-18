;;
;; dired tweak -> use as launcher
;;
;; - launch files with 'l' key
;;
;; - see also:
;;     http://omniorthogonal.blogspot.com/2008/05/useful-emacs-dired-launch-hack.html
;;     http://www.emacswiki.org/emacs/AnythingLauncher
;;

(defun dired-launch-command ()
  (interactive) 
  (if (eq system-type 'windows)
      (dired-map-over-marks
       (w32-shell-execute "open" (dired-get-filename) nil 1))
    ;; forego persistent *Async Shell Command* buffer
    (save-window-excursion
    ;; dired-do-async-shell-command has issues... but plays nicely with xdg-open
    ;; dired-do-shell-command is a drag since emacs locks until launched app terminates
      (dired-do-async-shell-command
       (case system-type
	 (gnu/linux "xdg-open") ; kde-open, gnome-open, ...
	 (darwin "open"))
       nil
       (dired-get-marked-files t current-prefix-arg)))))

(setq dired-load-hook
      (lambda (&rest ignore)
	(define-key dired-mode-map
	  "l" 'dired-launch-command)))
