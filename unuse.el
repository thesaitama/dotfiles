
;; ------------------------------------------------------------------------
;; html-mode (xhtml, html)

(setq auto-mode-alist
      (append '(("\\(\\.x?html?\\|iht\\)\\([.]?\\w+\\)*$" . html-mode))
       auto-mode-alist))

;; ------------------------------------------------------------------------
;; css-mode

(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))

;; ------------------------------------------------------------------------
;; mmm-mode (html, php)

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
;(set-face-background 'mmm-default-submode-face nil) ;背景色が不要な場合
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")))
(mmm-add-mode-ext-class nil "\\(\\.x?html?\\|php\\)(\\..+\\)?$" 'embedded-css)
(mmm-add-classes
 '((embedded-js
    :submode javascript-mode
    :front "<script[^>]*>"
    :back "</script>")))
(mmm-add-mode-ext-class nil "\\(\\.x?html?\\|php\\)(\\..+\\)?$" 'embedded-js)

;; ------------------------------------------------------------------------
;; tabbar

(require 'tabbar)
(tabbar-mode 0)
(tabbar-mwheel-mode nil)                  ;; マウスホイール無効
(setq tabbar-buffer-groups-function nil)  ;; グループ無効
(setq tabbar-use-images nil)              ;; 画像を使わない
(global-set-key (kbd "<f7>") 'tabbar-backward-tab)
(global-set-key (kbd "<f8>") 'tabbar-forward-tab)
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
(setq tabbar-separator '(2.0))
(set-face-attribute
 'tabbar-default nil
 :background "black" :foreground "white")
(set-face-attribute
 'tabbar-selected nil
 :background "white" :foreground "black" :box nil
 )
(set-face-attribute
 'tabbar-modified nil
 :background "black"  :foreground "red" :box nil
 )
(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
                     ((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; ------------------------------------------------------------------------
;; elscreen

(setq elscreen-prefix-key (kbd "M-z"))
(elscreen-start)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;; ------------------------------------------------------------------------
;; psgml

(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; ------------------------------------------------------------------------
;; xml-mode (RELAX, RELAX NG, iht)

(setq auto-mode-alist
      (append '(("\\.\\(xml\\|rlx\\|pml\\|rng\\)$" . xml-mode))
       auto-mode-alist))