;; Valerio's emacs configuration


;; 			;;
;;	BASE SETTING	;;
;;			;;

;; style setting
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; path setting
(setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin/"))
(setq exec-path (append exec-path '("~/.cargo/bin/")))

;; melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; my packages
(require 'cl-lib)

(defvar my-packages '(gruvbox-theme ;
		      racer ;
		      projectile ;
		      flycheck-rust ;
		      dashboard ;
		      company ;
		      all-the-icons ;
		      rust-mode ;
		      doom-modeline ;
		      rainbow-delimiters ;
		      neotree ;
		      minions )) ;

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; load theme
(load-theme 'gruvbox t)

;; set logo
(when (not (file-exists-p "~/.emacs.d/emacs.png"))
  (shell-command "wget https://www.gnu.org/software/emacs/images/emacs.png -O ~/.emacs.d/emacs.png"))

(setq dashboard-startup-banner "~/.emacs.d/emacs.png")


;; set default font
(add-to-list 'default-frame-alist '(font . "Hack-10" ))
(set-face-attribute 'default t :font "Hack-10" )

;;			;;
;;	OTHER SETTING	;;
;;			;;

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; minions
(minions-mode 1)

;; doom-modeline
(require 'doom-modeline)
(doom-modeline-mode 1)

;; page-break-lines
(require 'page-break-lines)

;; projectile
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; all-the-icons
(require 'all-the-icons)

;; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-set-heading-icons t)

;; rust-mode
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(setq rust-format-on-save t)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(setq company-minimum-prefix-length 1)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (spacemacs-theme racer projectile flycheck-rust dashboard company all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
