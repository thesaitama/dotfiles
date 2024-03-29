;;; .emacs.el --- thesaitama Emacs configuration

;;  _   _                     _ _
;; | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
;; | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
;; | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
;;  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

;;; Commentary:
;;
;; thesaitama@ .emacs.el
;; Last Update: 2023-02-12 23:29:32
;; tested with: Emacs 26.1-27.2 (or 22.1), macOS 11.3, Windows 10

;; install
;; > sudo apt-get install libncurses5-dev libgnutls-openssl27 libgnutls28-dev
;; > wget http://ftpmirror.gnu.org/emacs/emacs-26.1.tar.gz
;; > tar xzvf emacs-26.1.tar.gz
;; > cd emacs-26.1
;; > ./configure --without-x
;; > make
;; > sudo make install

;;; Code:

;; temporally solution for package install error
;; (setq package-check-signature nil)

;; (package-initialize)

;; ------------------------------------------------------------------------
;; utility function

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(defun load-if-exist (file-path)
  "Load file if FILE-PATH is exist."
  (if (file-exists-p file-path)
      (load file-path))
  )

;; ------------------------------------------------------------------------
;; switch configure by emacs version

(if (>= emacs-major-version 25)
    (load-if-exist "~/dotfiles/elisp/init-fullmacs.el")
  (load-if-exist "~/dotfiles/elisp/init-minimacs.el")
  )

;; ------------------------------------------------------------------------
;; custom-set-faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "SkyBlue"))))
 '(bm-face ((t (:background "SpringGreen4"))))
 '(bm-fringe-face ((t (:background "SpringGreen4"))))
 '(company-preview-common ((t (:background nil :foreground "LightGray" :underline t))))
 '(company-scrollbar-bg ((t (:background "Gray40"))))
 '(company-scrollbar-fg ((t (:background "orange"))))
 '(company-tooltip ((t (:foreground "black" :background "LightGray"))))
 '(company-tooltip-common ((t (:foreground "black" :background "LightGray"))))
 '(company-tooltip-common-selection ((t (:foreground "white" :background "SteelBlue"))))
 '(company-tooltip-selection ((t (:foreground "black" :background "SteelBlue"))))
 '(custom-face-tag ((t (:foreground "SkyBlue"))))
 '(custom-group-tag ((t (:foreground "orange"))))
 '(custom-state ((t (:foreground "Gray70"))))
 '(custom-variable-tag ((t (:foreground "orange"))))
 '(diff-added ((((type tty)) (:foreground "green"))))
 '(diff-refine-added ((t (:foreground "white" :background "LimeGreen"))))
 '(diff-refine-removed ((t (:foreground "white" :background "red"))))
 '(diff-removed ((((type tty)) (:foreground "red"))))
 '(dired-header ((t (:background "DarkCyan" :foreground "white"))))
 '(dired-flagged ((t (:foreground "IndianRed"))))
 '(dired-subtree-depth-1-face ((t (:background "Gray19"))))
 '(dired-subtree-depth-2-face ((t (:background "Gray20"))))
 '(dired-subtree-depth-3-face ((t (:background "Gray21"))))
 '(dired-subtree-depth-4-face ((t (:background "Gray22"))))
 '(dired-subtree-depth-5-face ((t (:background "Gray23"))))
 '(dired-subtree-depth-6-face ((t (:background "Gray24"))))
 '(ediff-current-diff-A ((t (:foreground "black" :background "LimeGreen"))))
 '(ediff-current-diff-Ancestor ((t (:foreground "black" :background "DarkCyan"))))
 '(ediff-current-diff-B ((t (:foreground "black" :background "red"))))
 '(ediff-current-diff-C ((t (:foreground "black" :background "orange"))))
 '(ediff-even-diff-A ((t (:foreground "white" :background "Gray30"))))
 '(ediff-even-diff-Ancestor ((t (:foreground "white" :background "Gray25"))))
 '(ediff-even-diff-B ((t (:foreground "white" :background "Gray35"))))
 '(ediff-even-diff-C ((t (:foreground "white" :background "Gray40"))))
 '(ediff-fine-diff-A ((t (:foreground "white" :background "LimeGreen"))))
 '(ediff-fine-diff-Ancestor ((t (:foreground "white" :background "DarkCyan"))))
 '(ediff-fine-diff-B ((t (:foreground "white" :background "red"))))
 '(ediff-fine-diff-C ((t (:foreground "white" :background "orange"))))
 '(ediff-odd-diff-A ((t (:foreground "white" :background "Gray30"))))
 '(ediff-odd-diff-Ancestor ((t (:foreground "white" :background "Gray25"))))
 '(ediff-odd-diff-B ((t (:foreground "white" :background "Gray35"))))
 '(ediff-odd-diff-C ((t (:foreground "white" :background "Gray40"))))
 '(sh-quoted-exec ((t (:foreground "HotPink"))))
 '(smerge-upper ((t (:background "Gray20"))))
 '(smerge-lower ((t (:background "Gray25"))))
 '(smerge-base((t (:background "Gray15"))))
 '(smerge-refined-added ((t (:foreground "white" :background "DarkCyan"))))
 '(smerge-refined-removed ((t (:foreground "white" :background "red"))))
 '(smerge-refined-changed ((t (:background "Gray30"))))
 '(smerge-markers ((t (:background "Gray40"))))
 '(ein:cell-input-area ((t (:background "Gray22"))))
 '(ein:cell-output-area ((t (:background "Gray10"))))
 '(elscreen-tab-background-face ((t (:background "Gray10" :foreground "Gray90"))))
 '(elscreen-tab-control-face ((t (:background "Gray20" :foreground "Gray90"))))
 '(elscreen-tab-current-screen-face ((t (:background "Gray80" :foreground "Gray20"))))
 '(elscreen-tab-other-screen-face ((t (:background "Gray25" :foreground "Gray80"))))
 '(escape-glyph ((t (:foreground "SkyBlue" :background "Gray25"))))
 '(git-gutter:added ((t (:foreground "LimeGreen"))))
 '(git-gutter:delete ((t (:foreground "red"))))
 '(git-gutter:modified ((t (:foreground "LightSkyBlue"))))
 '(header-line ((t (:background "Gray40" :foreground "Gray85"))))
 '(helm-buffer-directory ((t (:background "DarkCyan" :foreground "white"))))
 '(helm-buffer-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(helm-buffer-process ((t (:inherit font-lock-builtin-face :foreground "MediumPurple2"))))
 '(helm-ff-directory ((t (:background "Gray25" :foreground "orange"))))
 '(helm-ff-dotted-directory ((t (:background "Gray25" :foreground "white"))))
 '(helm-ff-executable ((t (:inherit font-lock-builtin-face :foreground "cyan"))))
 '(helm-ff-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(helm-ff-symlink ((t (:inherit font-lock-builtin-face :foreground "MediumPurple2"))))
 '(helm-grep-file ((t (:inherit font-lock-builtin-face :underline t :foreground "cyan"))))
 '(helm-grep-match ((t (:background "LightCyan" :foreground "black"))))
 '(helm-header ((t (:background "Gray40" :foreground "Gray80"))))
 '(helm-match ((t (:foreground "SkyBlue"))))
 '(helm-selection ((t (:background "Gray30"))))
 '(helm-selection-line ((t (:background "Gray20"))))
 '(helm-source-header ((t (:background "DarkCyan" :foreground "white"))))
 '(helm-visible-mark ((t (:background "Gray40"))))
 '(highlight-indentation-current-column-face ((t (:background "Gray40"))))
 '(highlight-indentation-face ((t (:background "Gray25"))))
 '(highlight-symbol-face ((t (:background "Gray25"))))
 '(hl-line ((t (:background "Gray19"))))
 '(holiday ((t (:background "pink" :foreground "black"))))
 '(hs-face ((t (:background "Gray40"))))
 '(imenu-list-entry-face-0 ((t (:inherit outline-1))))
 '(imenu-list-entry-face-1 ((t (:inherit outline-2))))
 '(imenu-list-entry-face-2 ((t (:inherit outline-3))))
 '(imenu-list-entry-face-3 ((t (:inherit outline-4))))
 '(isearch ((t (:background "LightPink" :foreground "black"))))
 '(japanese-holiday-saturday ((t (:background "cyan" :foreground "black"))))
 '(lazy-highlight ((t (:background "LightCyan" :foreground "black"))))
 '(link ((t (:foreground "cyan"))))
 '(linum ((t (:inherit (shadow default) :background "Gray22"))))
 '(magit-branch-local ((t (:foreground "MediumPurple2"))))
 '(magit-branch-remote ((t (:foreground "cyan"))))
 '(magit-context-highlight ((t (:background "Gray23"))))
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((t (:background "Gray23"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-log-author ((t (:foreground "MediumPurple2"))))
 '(magit-section-heading ((t (:foreground "cyan" :weight bold))))
 '(magit-section-highlight ((t (:background "Gray23"))))
 '(markdown-code-face ((t (:inherit default :background "Gray20"))))
 '(markdown-header-delimiter-face ((t (:inherit org-mode-line-clock))))
 '(markdown-header-face-1 ((t (:inherit outline-1))))
 '(markdown-header-face-2 ((t (:inherit outline-2))))
 '(markdown-header-face-3 ((t (:inherit outline-3))))
 '(markdown-header-face-4 ((t (:inherit outline-4))))
 '(markdown-header-face-5 ((t (:inherit outline-5))))
 '(markdown-header-face-6 ((t (:inherit outline-6))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(markdown-markup-face ((t (:foreground "green"))))
 '(menu ((t (:background "Gray30"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(mlc-mode-line-char-format ((t (:foreground "Gray90" :background "Gray40"))))
 '(mlc-mode-line-char-format-code ((t (:foreground "Gray80"))))
 '(newsticker-enclosure-face ((((type tty)) (:background "oragne" :foreground "white"))))
 '(newsticker-treeview-face ((((type tty)) (:foreground "Gray80"))))
 '(newsticker-treeview-new-face ((((type tty)) (:foreground "white" :weight bold))))
 '(newsticker-treeview-selection-face ((((type tty)) (:background "Gray25"))))
 '(nxml-attribute-local-name ((t (:foreground "LightBlue"))))
 '(nxml-attribute-value ((t (:foreground "Goldenrod"))))
 '(nxml-cdata-section-content ((t (:foreground "gray"))))
 '(nxml-comment-content ((t (:inherit font-lock-comment-face))))
 '(nxml-comment-delimiter ((t (:foreground "green"))))
 '(nxml-element-local-name ((t (:foreground "cyan"))))
 '(nxml-entity-ref-delimiter ((t (:foreground "red"))))
 '(nxml-entity-ref-name ((t (:foreground "red"))))
 '(nxml-name-face ((t (:foreground "cyan"))))
 '(nxml-tag-delimiter ((t (:foreground "LightBlue"))))
 '(org-agenda-current-time ((t (:foreground "SkyBlue"))))
 '(org-agenda-date ((t (:foreground "white"))))
 '(org-agenda-date-today ((t (:foreground "orange" :weight bold))))
 '(org-agenda-date-weekend ((t (:foreground "bisque" :background "Gray23"))))
 '(org-agenda-structure ((t (:background "DarkCyan" :foreground "white"))))
 '(org-date ((t (:foreground "LightBlue"))))
 '(org-done ((t (:foreground "LightGreen" :weight bold))))
 '(org-drawer ((t (:foreground "SkyBlue"))))
 '(org-scheduled ((t (:foreground "green"))))
 '(org-scheduled-today ((t (:foreground "SteelBlue1"))))
 '(org-special-keyword ((t (:foreground "yellow"))))
 '(org-time-grid ((t (:foreground "Gray70"))))
 '(org-todo ((t (:foreground "LightSkyBlue" :weight bold))))
 '(org-upcoming-deadline ((t (:foreground "HotPink" :weight bold))))
 '(org-warning ((t (:foreground "pink"))))
 '(outline-1 ((t (:foreground "orange" :weight bold :underline t))))
 '(outline-2 ((t (:foreground "beige" :weight bold))))
 '(outline-3 ((t (:foreground "PaleTurquoise" :weight bold))))
 '(outline-4 ((t (:foreground "LightSkyBlue" :weight bold))))
 '(outline-5 ((t (:foreground "DeepSkyBlue" :weight bold))))
 '(outline-6 ((t (:foreground "SkyBlue"))))
 '(outline-7 ((t (:foreground "LightBlue"))))
 '(package ((t (:foreground "DodgerBlue"))))
 '(package-name ((t (:foreground "DodgerBlue"))))
 '(pulse-highlight-face ((t (:background "Gray35"))))
 '(pulse-highlight-start-face ((t (:background "Gray35"))))
 '(rainbow-delimiters-mismatched-face ((t (:background "red" :foreground "white"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "red" :foreground "white"))))
 '(region ((t (:background "Gray40"))))
 '(rst-level-1 ((t (:inherit outline-1))))
 '(rst-level-2 ((t (:inherit outline-2))))
 '(rst-level-3 ((t (:inherit outline-3))))
 '(rst-level-4 ((t (:inherit outline-4))))
 '(rst-level-5 ((t (:inherit outline-5))))
 '(rst-level-6 ((t (:inherit outline-6))))
 '(show-paren-match ((t (:background "DodgerBlue" :foreground "white"))))
 '(show-paren-match-expression ((t (:background "Gray25"))))
 '(show-paren-match-face ((t (:background "black" :foreground "white"))))
 '(show-paren-mismatch ((t (:background "red"))))
 '(sml/col-number ((t (:foreground "Gray75"))))
 '(sml/folder ((t (:foreground "White"))))
 '(sml/git ((t (:foreground "LightBlue"))))
 '(sml/minor-modes ((t (:foreground "Gray75"))))
 '(sml/mule-info ((t (:foreground "SkyBlue"))))
 '(sml/prefix ((t (:foreground "orange"))))
 '(term-color-black ((t (:foreground "#444945"))))
 '(term-color-blue ((t (:foreground "#5cabdc"))))
 '(term-color-cyan ((t (:foreground "#44aabb"))))
 '(term-color-green ((t (:foreground "#8cc16e"))))
 '(term-color-magenta ((t (:foreground "#b595cf"))))
 '(term-color-red ((t (:foreground "#da5774"))))
 '(term-color-white ((t (:foreground "#fbfbf8"))))
 '(term-color-yellow ((t (:foreground "#eebf36"))))
 '(term-default-bg-color ((t (:inherit term-color-black))))
 '(term-default-fg-color ((t (:inherit term-color-white))))
 '(tide-hl-identifier-face ((t (:background "Gray28"))))
 '(tool-bar ((t (:foreground "cyan"))))
 '(tty-menu-disabled-face ((t (:background "Gray45" :foreground "Gray10"))))
 '(tty-menu-enabled-face ((t (:background "Gray45" :foreground "White"))))
 '(tty-menu-selected-face ((t (:background "SteelBlue" :foreground "White"))))
 '(w3m-header-line-location-content ((t (:background "Gray40"))))
 '(w3m-header-line-location-title ((t (:background "Gray30"))))
 '(w3m-tab-background ((t (:background "Gray20"))))
 '(web-mode-comment-face ((t (:inherit font-lock-comment-face))))
 '(web-mode-css-at-rule-face ((t (:foreground "color-99"))))
 '(web-mode-css-pseudo-class ((t (:foreground "DodgerBlue"))))
 '(web-mode-css-selector-face ((t (:foreground "DodgerBlue"))))
 '(web-mode-current-element-highlight-face ((t (:background "Gray25"))))
 '(web-mode-doctype-face ((t (:inherit font-lock-doc-face))))
 '(web-mode-html-attr-equal-face ((t (:foreground "white"))))
 '(web-mode-html-attr-name-face ((t (:foreground "LightBlue"))))
 '(web-mode-html-attr-value-face ((t (:inherit font-lock-string-face))))
 '(web-mode-html-entity-face ((t (:foreground "DodgerBlue"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "LightBlue"))))
 '(web-mode-html-tag-face ((t (:foreground "cyan"))))
 '(web-mode-server-comment-face ((t (:foreground "green"))))
 '(which-func ((t (:foreground "ivory"))))
 '(which-key-command-description-face ((t (:foreground "white")))))

;; ------------------------------------------------------------------------
;; custom-set-variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(edit-indirect smooth-scrolling apache-mode nginx-mode add-node-modules-path markdown-toc request json-navigator tuareg vi-tilde-fringe gitignore-mode wanderlust flyspell-popup xterm-color uimage esup peep-dired recentf-ext helm-google google-this dumb-jump textile-mode osx-clipboard pippel powershell flycheck-plantuml ssh plantuml-mode smooth-scroll tide helm-dash vimrc-mode helm-flyspell howdoi google-translate xah-lookup osx-trash japanese-holidays dired-subtree dired-narrow w3m smart-mode-line which-key scratch-pop shell-pop multi-term popwin elscreen emamux magit-find-file magit helm-projectile projectile yagist qiita helm-c-yasnippet yasnippet-snippets restclient-helm restclient helm-bm bm helm-descbinds helm-gtags helm-ag helm-smex imenu-list imenu-anywhere imenus flycheck-popup-tip flycheck elpy jedi python-mode typescript-mode json-mode js2-refactor php-eldoc web-mode rainbow-delimiters rainbow-mode comment-tags undo-tree foreign-regexp highlight-symbol expand-region anzu ac-helm ac-php ac-js2 ac-html quickrun editorconfig sequential-command fuzzy avy pos-tip auto-complete package-utils exec-path-from-shell 0xc)))

;; ------------------------------------------------------------------------

(provide '.emacs.el)
;;; .emacs.el ends here
