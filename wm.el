;; wm.el - Simple Workspace Management
;; A minimal workspace system for Emacs inspired by i3/tiling WMs.

(defvar wm-workspaces (make-hash-table :test 'equal)
  "Hash table mapping workspace number (1-9) to window configuration.")

(defvar wm-current-workspace nil
  "Currently active workspace number, or nil if none.")

(defun wm--get-scratch-buffer (n)
  "Get or create the scratch buffer for workspace N."
  (let ((name (format "*scratch-ws-%d*" n)))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (lisp-interaction-mode)
          (current-buffer)))))

(defun wm--init-workspaces ()
  "Initialize default workspaces 1-5 with their scratch buffers."
  (let ((orig-config (current-window-configuration)))
    (dolist (n '(1 2 3 4 9))
      (delete-other-windows)
      (switch-to-buffer (wm--get-scratch-buffer n))
      (puthash n (current-window-configuration) wm-workspaces))
    ;; Restore original state and set workspace 1 as current
    (set-window-configuration (gethash 1 wm-workspaces))
    (setq wm-current-workspace 1)))

(defun wm-switch-to-workspace (n)
  "Switch to workspace N.
If currently in a workspace, save current config first.
If workspace N exists, restore it. Otherwise show a message."
  (interactive "nWorkspace number: ")
  ;; Save current workspace if we're in one
  (when wm-current-workspace
    (puthash wm-current-workspace (current-window-configuration) wm-workspaces))
  ;; Try to switch to workspace N
  (let ((config (gethash n wm-workspaces)))
    (if config
        (progn
          (set-window-configuration config)
          (setq wm-current-workspace n)
          (message "Switched to workspace %d" n))
      (message "Workspace %d not defined" n))))

(defun wm-save-workspace (n)
  "Save current window configuration as workspace N."
  (interactive "nWorkspace number: ")
  (puthash n (current-window-configuration) wm-workspaces)
  (setq wm-current-workspace n)
  (message "Saved workspace %d" n))

;; Switch to workspace keybindings (M-1 through M-9)
(global-set-key (kbd "M-1") (lambda () (interactive) (wm-switch-to-workspace 1)))
(global-set-key (kbd "M-2") (lambda () (interactive) (wm-switch-to-workspace 2)))
(global-set-key (kbd "M-3") (lambda () (interactive) (wm-switch-to-workspace 3)))
(global-set-key (kbd "M-4") (lambda () (interactive) (wm-switch-to-workspace 4)))
(global-set-key (kbd "M-5") (lambda () (interactive) (wm-switch-to-workspace 5)))
(global-set-key (kbd "M-6") (lambda () (interactive) (wm-switch-to-workspace 6)))
(global-set-key (kbd "M-7") (lambda () (interactive) (wm-switch-to-workspace 7)))
(global-set-key (kbd "M-8") (lambda () (interactive) (wm-switch-to-workspace 8)))
(global-set-key (kbd "M-9") (lambda () (interactive) (wm-switch-to-workspace 9)))

(global-set-key (kbd "M-!") (lambda () (interactive) (wm-save-workspace 1)))
(global-set-key (kbd "M-@") (lambda () (interactive) (wm-save-workspace 2)))
(global-set-key (kbd "M-#") (lambda () (interactive) (wm-save-workspace 3)))
(global-set-key (kbd "M-$") (lambda () (interactive) (wm-save-workspace 4)))
(global-set-key (kbd "M-%") (lambda () (interactive) (wm-save-workspace 5)))
(global-set-key (kbd "M-^") (lambda () (interactive) (wm-save-workspace 6)))
(global-set-key (kbd "M-&") (lambda () (interactive) (wm-save-workspace 7)))
(global-set-key (kbd "M-*") (lambda () (interactive) (wm-save-workspace 8)))
(global-set-key (kbd "M-(") (lambda () (interactive) (wm-save-workspace 9)))

(wm--init-workspaces)

(provide 'wm)
