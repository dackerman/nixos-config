;; NO FRILLS
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t)

;; NO JUNK
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

(global-linum-mode 1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)


(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;(def default-font "DejaVu Sans Mono-17")


(package-initialize)
(require 'darcula-theme)
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
(set-face-attribute 'default t :font "DejaVu Sans Mono-10")
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
(set-frame-font "DejaVu Sans Mono-10" nil t)


(require 'haskell-mode-autoloads)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

(setq web-mode-enable-auto-pairing t)

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(require 'nix-mode)
(require 'magit)

(setq-default indent-tabs-mode nil)


(setq browse-url-browser-function 'browse-url-generic
      browser-url-generic-program "chromium-browser")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-chromium)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
