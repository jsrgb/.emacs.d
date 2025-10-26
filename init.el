;;; .emacs for terminal
;;; Commentary
;;; Code:

(when (version<  emacs-version "30.0")
  (error "Config not tested on v%s. Please use v30.0 or higher." emacs-version))

(setq default-frame-alist
      '((fullscreen . maximized)
        (font . "Lilex-16")
        (vertical-scroll-bars . nil)))

(setq backup-directory-alist '(("." . "~/.emacsbackups")))

(setq package-archives
      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")
        
        ("gnu-devel" . "https://elpa.gnu.org/devel/") ;; for eglot bleeding edge
        ))
(package-initialize)

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH"))
(setenv "LIBRARY_PATH" "/opt/homebrew/lib")

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) ;; Prevent double-loading packages
  (package-initialize))

(defvar pkg-refreshed nil)

(defun pm/use (package)
  (when (not pkg-refreshed)
    (setq pkg-refreshed t)
    (package-refresh-contents))
  (when (not (package-installed-p package))
    (package-install package)))

(load-theme 'zenburn t)

;;;
;;; Defaults
;;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode 0)
(column-number-mode)
(global-display-line-numbers-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(fringe-mode '(10 . 0))
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      scroll-conservatively 100
      display-time-day-and-date t
      package-enable-at-startup nil
      confirm-kill-emacs 'yes-or-no-p
      switch-to-buffer-obey-display-actions t)

(setq-default indicate-empty-lines t
              indent-tabs-mode nil
              tab-width 4)

(setq native-comp-async-report-warnings-errors 'silent)

(setopt display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(setq display-line-numbers-type 'absolute)
(global-display-line-numbers-mode +1)

(pm/use 'rg)
(rg-enable-default-bindings)

(global-set-key (kbd "C-c r") (lambda () (interactive) (recompile)))
(global-set-key (kbd "C-c `") 'next-error)

(use-package vertico
  :ensure t
  :init (vertico-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("806dd05c68b646416d686fc45d1ed7e6a173511e2548cd62150473fe5149f66c"
     default))
 '(package-selected-packages
   '(consult exec-path-from-shell glsl-mode json-mode lua-mode magit
             move-text multiple-cursors rg rust-mode typescript-mode
             vertico wgsl-mode yaml-mode zenburn-theme zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;
;;; Window and Layout Handling
;;;
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-below) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-c b") 'switch-to-prev-buffer)
(global-set-key (kbd "C-c f") 'switch-to-next-buffer)
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-`"))
(global-set-key (kbd "M-o") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))


(defun save-layout-delete-other-windows ()
  "Save the current layout and focus on current window."
  (interactive)
  (window-configuration-to-register ?-)
  (delete-other-windows))

(defun goto-saved-layout ()
  (interactive)
  "Go to saved window layout"
  (jump-to-register ?-))

(global-set-key (kbd "C-;") 'switch-to-buffer)
(global-set-key (kbd "C-x 1") 'save-layout-delete-other-windows)
(global-set-key (kbd "C-c w") 'goto-saved-layout)
(global-unset-key (kbd "C-x `"))
(global-unset-key (kbd "C-z"))

;;;
;;; Editing
;;;
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(pm/use 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(pm/use 'move-text)
(move-text-default-bindings)

;;;
;;; Programming Modes
;;;
(setq python-indent-offset 4)
(pm/use 'yaml-mode)
(pm/use 'json-mode)
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 'js-tabwidth)))

(setq c-set-style "k&r")
(setq c-basic-offset 4)

(pm/use 'magit)
(pm/use 'magit-section)

(pm/use 'wgsl-mode)
(pm/use 'glsl-mode)
(pm/use 'zig-mode)
(pm/use 'rust-mode)

(defun js-ts-indent ()
  (setq-local tab-width 2)
  (setq-local js-indent-level 2)
  (setq-local typescript-indent-level 2))

(add-hook 'js-mode-hook #'js-ts-indent)
(add-hook 'typescript-mode-hook #'js-ts-indent)
(add-hook 'html-mode-hook #'js-ts-indent)

(pm/use 'typescript-mode)

;;;
;;; UI
;;;

(pm/use 'which-key)
(which-key-mode)

(setq gc-cons-threshold (* 100 1024 1024))

;;
;; LSP
;;

(add-hook 'programming-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook 'eglot-ensure)

(with-eval-after-load 'eglot
  (advice-add 'jsonrpc--log-event :override #'ignore)
  (add-to-list 'eglot-server-programs
               '((typescript-mode tsx-ts-mode typescript-ts-mode)
                 . ("vtsls" "--stdio"))
               '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "check"))))))

;;
;; Flymake
;;
(global-set-key (kbd "C-c e b") 'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c e p") 'flymake-show-project-diagnostics)


(use-package consult
  :ensure t
  :bind
  :config
  (global-set-key (kbd "M-p") 'consult-fd)
  (global-set-key (kbd "M-O") 'consult-imenu)
  (global-set-key (kbd "M-F") 'consult-ripgrep)
  (global-set-key (kbd "M-s") 'consult-line)
  )
