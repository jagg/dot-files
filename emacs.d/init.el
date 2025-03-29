;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Emacs Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/elpaca-cfg.el")
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Setup exec-path and PATH environment variable, in case shell has failed to do so
(let ((paths (mapcar (lambda (i) (concat (getenv "HOME") "/" i))
                     '("bin" "emacs/bin" ".cabal/bin" "node/bin"))))
  (setenv "PATH" (apply 'concat
                        (append (mapcar (lambda (i) (concat i ":")) paths)
                                (list (getenv "PATH")))))
  (dolist (path paths) (when (file-directory-p path)
                         (add-to-list 'exec-path path))))

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(global-display-line-numbers-mode)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/bak"))))

;; https://typeof.net/Iosevka/
(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height 140
                    :slant 'normal
                    :weight 'light)

(show-paren-mode 1)

(set-frame-parameter nil 'alpha-background 50)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Emacs Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package key-chord
  :ensure t
  :demand t
  :init (key-chord-mode 1))

(use-package evil
  :ensure t
  :demand t
  :init (evil-mode 1))

(use-package goto-chg
  :ensure t
  :demand t)

(use-package all-the-icons
  :ensure t
  :demand t
  :if (display-graphic-p))

(use-package nerd-icons
  :ensure t
  :demand t)

(use-package doom-modeline
  :ensure t
  :demand t
  :init (doom-modeline-mode 1))

(use-package which-key
  :ensure t
  :demand t
  :init (which-key-mode))

(use-package neotree
  :ensure t
  :demand t)

(use-package doom-themes
  :ensure t
  :demand t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :ensure t
  :demand t
  :hook '(prog-mode))

(use-package ivy
  :ensure t
  :demand t
  :init (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel 
  :ensure t
  :demand t)

(use-package swiper 
  :ensure t
  :demand t)

(use-package projectile 
  :ensure t
  :demand t
  :init (projectile-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :demand t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  :hook ((tuareg-mode . lsp)
         (caml-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :demand t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil))

(use-package lsp-treemacs
  :ensure t
  :demand t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :demand t
  :ensure t)


;; When the completion candidates are shown, press <f1> to display the
;; documentation for the selected candidate, or C-w to see its
;; source. Not all back-ends support this.

(use-package company
  :ensure t
  :demand t
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  :hook (prog-mode . company-mode))

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++
;; See https://tuhdo.github.io/c-ide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-default-style "bsd" c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(add-hook 'c++-mode-hook
          (lambda ()
            (lsp-mode)
            (c-set-style "bsd")
            (setq c-basic-offset 2)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)))
 
(add-hook 'c-mode-hook
          (lambda ()
            (lsp-mode)
            (c-set-style "bsd")
            (setq c-basic-offset 2)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp
;; See https://lispcookbook.github.io/cl-cookbook/emacs-ide.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sly 
  :demand t
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package sly-asdf
  :demand t
  :ensure t)

(use-package paredit
  :ensure t
  :demand t
  :hook '(lisp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OCaml 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((opam-bin (expand-file-name "~/.opam/5.3.0/bin")))
  (unless (member opam-bin exec-path)
    (add-to-list 'exec-path opam-bin))
  (setenv "PATH" (concat opam-bin ":" (getenv "PATH"))))


(use-package utop
   :ensure t
   :config
   (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(use-package ocp-indent
  :ensure t)

(use-package dune
  :ensure t)

(use-package tuareg
   :ensure t
   :mode (("\\.ocamlinit\\'" . tuareg-mode)))

;; (use-package dune
;;   :ensure t)

;; (use-package merlin
;;   :ensure t
;;   :config
;;   (add-hook 'tuareg-mode-hook #'merlin-mode)
;;   (add-hook 'merlin-mode-hook #'company-mode)
;;   ;; we're using flycheck instead
;;   (setq merlin-error-after-save nil))

;; (use-package merlin-eldoc
;;   :ensure t
;;   :hook ((tuareg-mode) . merlin-eldoc-setup))

;; (use-package flycheck-ocaml
;;   :ensure t
;;   :config
;;   (flycheck-ocaml-setup))

;; (use-package utop
;;   :ensure t
;;   :config
;;   (add-hook 'tuareg-mode-hook #'utop-minor-mode))


;; ;; (setq utop-command "dune utop")
;; (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
;; (add-hook 'tuareg-mode-hook #'merlin-mode)
;; (add-hook 'merlin-mode-hook #'company-mode)
;;   ;
;; (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
;;       (when (and opam-share (file-directory-p opam-share))
;;        ;; Register Merlin
;;        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;        (autoload 'merlin-mode "merlin" nil t nil)
;;        ;; Automatically start it in OCaml buffers
;;        (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;        (add-hook 'caml-mode-hook 'merlin-mode t)
;;        ;; Use opam switch to lookup ocamlmerlin binary
;;        (setq merlin-command 'opam)
;;        ;; To easily change opam switches within a given Emacs session, you can
;;        ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
;;        ;; and use one of its "OPSW" menus.
;;        ))

;; ;; https://github.com/bufordrat/monoconfig/blob/main/emacs/general-init.el#L157-L163
;; (defun my-utop-workaround (orig-fun name &rest args)
;;   (if (not (equal name "OCaml"))
;;       (apply orig-fun name args)
;;     (let ((process-connection-type nil))
;;       (apply orig-fun name args))))

;; (advice-add 'make-comint :around #'my-utop-workaround)

;; ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(elpaca (general :wait t))
(evil-set-leader nil (kbd "SPC"))
(key-chord-define evil-insert-state-map  "jj" 'evil-normal-state)

(evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>h") 'ff-find-other-file)
(evil-define-key 'normal 'global (kbd "<leader>b") 'compile)
(evil-define-key 'normal 'global (kbd "<leader>s") 'swiper)
(evil-define-key 'normal 'global (kbd "<leader>f") 'counsel-git-grep)
(evil-define-key 'normal 'global (kbd "<leader>e") 'lsp-treemacs-errors-list)
(evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
(evil-define-key 'normal 'global (kbd "<leader>d") 'eldoc-doc-buffer)

(evil-define-key 'normal 'global (kbd "<leader>l g") 'sly-edit-definition)
(evil-define-key 'normal 'global (kbd "<leader>l u") 'sly-edit-uses)
(evil-define-key 'normal 'global (kbd "<leader>l b") 'sly-edit-definition-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Stuff 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(merlin-occurrence-face ((t (:background "gray20" :foreground "white"))))
 '(merlin-occurrences-face ((t (:background "gray20" :foreground "green"))))
 '(merlin-type-face ((t (:inherit caml-types-expr-face)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-log-types '((use-package))))


