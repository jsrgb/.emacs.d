;;; .emacs
;;; Commentary
;;; Code:

(when (version<  emacs-version "30.1")
  (error "Config not tested on v%s. Please use v30.1 or higher." emacs-version))

(setq default-frame-alist
      '((fullscreen . maximized)
        (font . "Iosevka-18")
        (vertical-scroll-bars . nil)))


(setq backup-directory-alist '(("." . "~/.emacsbackups")))

(setq package-archives
      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")
        
        ("gnu-devel" . "https://elpa.gnu.org/devel/") ;; for eglot bleeding edge
        ))
(package-initialize)



;; (exec-path-from-shell-initialize)
;; (exec-path-from-shell-copy-envs '("PATH"))
;; (setenv "LIBRARY_PATH" "/opt/homebrew/lib")

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

(pm/use 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
(global-hl-line-mode)
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

;;
;; Local packages
;;
(add-to-list 'load-path "~/.emacs.d/custom/")

(pm/use 'base16-theme)
(pm/use 'doom-themes)
(load-theme 'doom-opera t)

(setq native-comp-async-report-warnings-errors 'silent)

(setopt display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;;;
;;; OS Specific
;;;
(defvar cfg-loc "")
(set-frame-font "Terminus (TTF) 16" nil t)

(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas-12")
  (setq cfg-loc "C:/Users/ja/AppData/Roaming/.emacs.d/init.el")
  (setq default-directory "C:/Workspace/"))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Iosevka-20" :weight 'medium)
  (setq mac-command-modifier 'meta)
  (setq cfg-loc "~/.emacs.d/init.el"))

(when (eq system-type 'gnu/linux)
  (setq cfg-loc "~/.emacs.d/init.el")
  (set-face-attribute 'default nil :font "Hack-12"))



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
;;(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))


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
;;; Projects

;;;
;;; Compilation
;;;
(global-set-key (kbd "C-c r") (lambda () (interactive) (recompile)))
(global-set-key (kbd "C-c `") 'next-error)

;;;
;;; Misc
;;;
(defun cfg ()
  "Open the `.emacs` or `init.el`."
  (interactive)
  (find-file cfg-loc))

;;;
;;; UI
;;;

(pm/use 'which-key)
(which-key-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(with-eval-after-load 'flymake
  ;; Do NOT extend diagnostics to whole defun
  (setq flymake-diagnostic-function 'flymake-diag-region))

(cl-defmethod eglot-handle-notification :after
  (_server (_method (eql textDocument/publishDiagnostics)) &key uri
           &allow-other-keys)
  (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
    (with-current-buffer buffer
      (if (and (eq nil flymake-no-changes-timeout)
               (not (buffer-modified-p)))
          (flymake-start t)))))

(global-set-key (kbd "C-.") #'completion-at-point)

;;
;; Flymake
;;
(global-set-key (kbd "C-c e b") 'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c e p") 'flymake-show-project-diagnostics)

;;
;; Org
;;
(load-file "~/.emacs.d/om.el")
;;(pm/use 'bufferlo)

;;
;; Debug
;;
(pm/use 'dape)
(setq dape-buffer-window-arrangement 'right)
(setq dape-key-prefix (kbd "C-x C-a"))

;;
;; Kill-ring
;;
(pm/use 'browse-kill-ring)
(global-unset-key (kbd " M-y"))
(global-set-key (kbd " M-y") 'browse-kill-ring)

(add-to-list 'exec-path "/Users/j/.local/bin")

;;
;; LLMs
;;
(global-unset-key (kbd "<C-wheel-up>"))
(global-unset-key (kbd "<C-wheel-down>"))


(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)

(setq warning-minimum-level :error)

(pm/use 'rg)
(rg-enable-default-bindings)

;; Basic org-roam setup - just the essentials
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Workspace/notes")
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))

;; (load "~/.emacs.d/odin-mode.el")

(use-package vertico
  :ensure t
  :init (vertico-mode 1))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic)))))

(use-package consult
  :ensure t
  :bind
  :config
  (global-set-key (kbd "M-p") 'consult-fd)
  (global-set-key (kbd "M-O") 'consult-imenu)
  (global-set-key (kbd "M-F") 'consult-ripgrep)
  (global-set-key (kbd "M-s") 'consult-line)
  (global-set-key (kbd "M-g M-g") 'consult-goto-line)
  )

(use-package apheleia
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'apheleia-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto nil))

(add-hook 'markdown-mode-hook (lambda () (corfu-mode -1)))
(add-hook 'text-mode-hook (lambda () (corfu-mode -1)))

(with-eval-after-load 'consult
  (setq consult-async-min-input 1
        consult-async-input-debounce 0.01
        consult-async-input-throttle 0.05))

;; todo; tree sitter


(global-set-key (kbd "TAB") #'indent-for-tab-command)


(pm/use 'blamer)
(global-set-key (kbd "C-c g b") #'blamer-mode)
(pm/use 'diff-hl)
(global-set-key (kbd "C-c g d") #'diff-hl-mode)

(global-set-key (kbd "C-i") 'xref-go-forward)
(global-set-key (kbd "C-o") 'xref-go-back)


(setq-default line-spacing 3)
(setq-default cursor-type '(box . 100))
(setq-default hl-line-sticky-flag t)

(load-file "~/.emacs.d/wm.el")
(load-file "~/Workspace/emacs-eat/eat.el")

(pm/use 'elcord)
(require 'elcord)
(elcord-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d12b1d9b0498280f60e5ec92e5ecec4b5db5370d05e787bc7cc49eae6fb07bc0"
     "2ab8cb6d21d3aa5b821fa638c118892049796d693d1e6cd88cb0d3d7c3ed07fc"
     default))
 '(eglot-ignored-server-capabilities
   '(:documentHighlightProvider :documentLinkProvider :colorProvider
                                :foldingRangeProvider
                                :inlayHintProvider)))
