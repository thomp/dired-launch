;;;
;;; Launch file manager on a dired directory
;;;
(defun dired-launch-fm-on-dired-dir-v3 ()
  (interactive)
  (dired-launch-x-on-dired-dir (dired-launch-fm-launch-cmd)))

(defun dired-launch-fm-launch-cmd ()
  "Return a string."
  (cond
   ;; ((eq system-type 'cygwin)
   ;;  )
   ;; ((eq system-type 'darwin)
   ;;  )
   ((or (eq system-type 'gnu/linux)
        (eq system-type 'berkeley-unix)
        (eq window-system 'x))
    "dolphin"
    )
   ((eq system-type 'windows-nt)
    "explorer"
    )
   (t (error "%s is not supported" system-type))))

(defun dired-launch-x-on-dired-dir (launch-cmd)
  (cond
   ;; ((eq system-type 'cygwin)
   ;;  )
   ;; ((eq system-type 'darwin)
   ;;  )
   ((or (eq system-type 'gnu/linux)
        (eq system-type 'berkeley-unix)
        (eq window-system 'x))
    (dired-launch-call-process-on launch-cmd
                                  (expand-file-name default-directory)  ; dired-directory unreliable
                                  ))
   ((eq system-type 'windows-nt)
    (dat-launch-x--windows launch-cmd))
   (t (error "%s is not supported" system-type))))

;; windows-friendly
(defun dat-launch-x--windows (launch-cmd)
  (interactive)
  (let ((path (replace-regexp-in-string (regexp-quote "/")
                                        (string #x5c)
                                        default-directory
                                        t
                                        t)))
    (setf path (replace-regexp-in-string (regexp-quote "c:")
                                         "C:"
                                         path
                                         t
                                         t))
    (shell-command (concat launch-cmd
                           " "
                           path))))

(defun dired-launch-call-process-on (launch-cmd &rest args)
  ;; handle file names with spaces
  (apply #'call-process
         (append (list launch-cmd
                       nil              ; infile
                       0                ; async-ish
                       nil              ; display
                       )
                 args)))
