;;; minimancs.el --- thesaitama Emacs configuration

;;  _   _                     _ _
;; | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
;; | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
;; | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
;;  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

;;; Commentary:
;;
;; thesaitama@ minimacs.el
;; light weight Emacs setting
;;

;;; Code:

;; ------------------------------------------------------------------------
;; ido

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

;; ------------------------------------------------------------------------
;; load basic settings

(load "~/dotfiles/cnf-basics.el")

;; ------------------------------------------------------------------------
;; custom-set-faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((((type tty)) (:foreground "green"))))
 '(diff-removed ((((type tty)) (:foreground "red"))))
 '(dired-header ((t (:background "BrightBlue" :foreground "white"))))
 '(dired-subtree-depth-1-face ((t (:background "Gray19"))))
 '(dired-subtree-depth-2-face ((t (:background "Gray20"))))
 '(dired-subtree-depth-3-face ((t (:background "Gray21"))))
 '(dired-subtree-depth-4-face ((t (:background "Gray22"))))
 '(dired-subtree-depth-5-face ((t (:background "Gray23"))))
 '(dired-subtree-depth-6-face ((t (:background "Gray24"))))
 '(font-lock-doc-face ((t (:foreground "green"))))
 '(fringe ((t (:background "Gray12" :foreground "blue"))))
 '(highlight-symbol-face ((t (:background "Gray25"))))
 '(hl-line ((t (:background "color-236"))))
 '(isearch ((t (:background "LightPink" :foreground "black"))))
 '(link ((t (:foreground "blue"))))
 '(linum ((t (:inherit (shadow default) :background "Gray22"))))
 '(outline-1 ((t (:background "BrightBlue" :foreground "white"))))
 '(outline-2 ((t (:foreground "cyan"))))
 '(outline-3 ((t (:foreground "blue"))))
 '(outline-4 ((t (:foreground "goldenrod"))))
 '(package-name ((t (:foreground "blue"))))
 '(region ((t (:background "Gray40"))))
 '(tool-bar ((t (:foreground "cyan"))))
)

;; ------------------------------------------------------------------------

(provide 'minimacs.el)
;;; minimacs.el ends here
