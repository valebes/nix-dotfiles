;;; init.el --- Valerio Besozzi's init.el File For GNU Emacs

;;; Commentary:

;; My Emacs configuration

;;; Code:

;;; Enable packages repos

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
      
;;; Customization:
;; font
(add-to-list 'default-frame-alist
             '(font . "mononoki Nerd Font Mono-11"))
(set-face-attribute 'default t :font "mononoki Nerd Font Mono-11")
;; rust bin loc
(setq exec-path (append exec-path '("~/.cargo/bin")))

;; toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; theme
(load-theme 'gruvbox t)

;; direnv (for nix)
(require 'direnv)
(direnv-mode)

;; git
(global-set-key (kbd "C-x g") 'magit-status)

;; highlight-parentheses
(require 'highlight-parentheses)
(add-hook 'after-init-hook #'global-highlight-parentheses-mode)

;; flycheck (linter)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; flycheck-rust
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; project view
(projectile-mode +1)
(defvar projectile-mode-map)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key "\t" 'company-complete-common)

;; racer (suggestion for rust) + company (completion)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(defvar company-tooltip-align-annotations)
(setq company-tooltip-align-annotations t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(require 'page-break-lines)

(require 'ido)
(ido-mode t)

(require 'helm-config)

;; splash screen
(require 'dashboard)
(dashboard-setup-startup-hook)

;; Set the banner
(setq dashboard-startup-banner "~/.emacs.d/logo.png")
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-items '((recents . 5)
			(projects . 5)))

;; directory tree view
(require 'all-the-icons)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; magit (git plugin)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; mode-line
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-icon t)
(setq doom-modeline-env-version t)

;; end

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (direnv gruvbox-theme doom-modeline magit highlight-parentheses company racer flycheck-rust flycheck rust-mode neotree projectile dashboard helm))))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
