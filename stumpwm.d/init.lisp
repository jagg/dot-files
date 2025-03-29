(in-package :stumpwm)
;; https://config.phundrak.com/stumpwm.html

;; (require :swank)
;; (swank-loader:init)

 ;; (swank:create-server :port 4004
 ;;                     :style swank:*communication-style*
 ;;                     :dont-close t)

(setf *startup-message* "Hack and be merry! Control + t ? for Help!")

(ql:quickload :slynk)


(stumpwm:defcommand sly-start-server () ()
  "Start a slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:create-server :dont-close t))))
(stumpwm:defcommand sly-stop-server () ()
  "Stop current slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:stop-server 4005))))

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
            (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
              (when cmd
                (eval-command cmd t))))

(setf  *input-window-gravity* :center) ;; input window comes to the center
(setf  *message-window-gravity* :center) ;; message window to the center
(setf *mode-line-background-color* "#3b4252" 
      *mode-line-foreground-color* "#d8dee9")

(setf *window-format* "%n: %30t")
(setf *group-format* "%t")

(setf stumpwm:*screen-mode-line-format*
      (list "%n | %d | %w"))

;; turn on/off the mode line for the current head only.
(when *initializing* 
  (stumpwm:toggle-mode-line (stumpwm:current-screen)
                            (stumpwm:current-head)))

(define-key *root-map* (kbd "b") "colon1 exec firefox http://www.google.com/search?q=")
(define-key *root-map* (kbd "z") "colon1 exec zathura")

;; Thanks to:
;; https://config.phundrak.com/stumpwm/theme.html
;; Had to be manually installed, not in quicklisp anymore

(ql:quickload 'clx-truetype)
(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "Fira Code" :subfamily "Regular" :size 14))


(set-msg-border-width 3)
(set-border-color "#ccffd8")
(set-bg-color "#3b4252")
(set-fg-color "#d8dee9")


(load-module "swm-gaps")

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)

(when *initializing*
  (swm-gaps:toggle-gaps))

(stumpwm:run-shell-command "feh --bg-scale Pictures/acropolis.jpeg")

;; Keys
(define-key stumpwm:*top-map* (stumpwm:kbd "M-Left") "gprev")
(define-key stumpwm:*top-map* (stumpwm:kbd "M-Right") "gnext")


;; Transparency

(exec-shell-command "compton --config ~/.config/compton.conf &")

(defun hide-all-lower-windows (current last)
  (declare (ignore current last))
  (when (typep (current-group) 'stumpwm::tile-group)
    (mapc (lambda (win)
            (unless (eq win (stumpwm::frame-window
                             (stumpwm::window-frame win)))
              (stumpwm::hide-window win)))
          (group-windows (current-group)))))

(export 'hide-all-lower-windows)

(defcommand enable-hiding-lower-windows () ()
  "Enable a hook that hides all windows that aren't at the top of their frame.
This is primarily useful when using transparent windows so that only the focused
window is visible (letting your wallpaper show through instead of other windows)."
  (add-hook *focus-window-hook* 'hide-all-lower-windows))
