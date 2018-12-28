;;; cnf-webservice.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; xah-lookup

(setq xah-lookup-browser-function 'eww)

;; ------------------------------------------------------------------------
;; google-translate

(when (require 'google-translate nil t)
  (setq google-translate-default-source-language "ja")
  (setq google-translate-default-target-language "en")
  (setq google-translate-base-url "https://translate.google.com/translate_a/single")
  (setq google-translate--tkk-url "https://translate.google.com/")
  ;; (setq google-translate--tkk-debug t)

  (defvar google-translate-english-chars "[:ascii:]’“”–"
    "English characters list.")
  (defun google-translate-enja-or-jaen (&optional string)
    "Auto detection translate language by STRING."
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string "Google Translate: "))
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string)))
  (global-set-key (kbd "M-g t") 'google-translate-enja-or-jaen)
  )

;; ------------------------------------------------------------------------
;; web dictionary

(defvar eww-data)
(defun eww-current-url ()
  "Adapt by eww version."
  (if (boundp 'eww-current-url)
      eww-current-url                   ; Emacs 24.4
    (plist-get eww-data :url)))         ; Emacs 25

(defun eww-set-start-at (url-regexp search-regexp)
  "Show line with SEARCH-REGEXP by URL matched by URL-REGEXP."
  (when (string-match url-regexp (eww-current-url))
    (goto-char (point-min))
    (when (re-search-forward search-regexp nil t)
      (recenter 0))))

(defun region-or-read-string (prompt &optional initial history default inherit)
  "When region selected read it, otherwise call `read-string'."
  (if (not (region-active-p))
      (read-string prompt initial history default inherit)
    (prog1
        (buffer-substring-no-properties (region-beginning) (region-end))
      (deactivate-mark)
      (message ""))))

(defun eww-render--after (&rest _)
  (eww-set-start-at "www.weblio.jp" "^ *Weblio 辞書")
  ;; add another Site if you want
  )
(if (boundp 'eww-after-hook) ; 25.1
    (add-hook 'eww-after-render-hook 'eww-render--after)
  (advice-add 'eww-render :after 'eww-render--after)) ; 24.4

;;; weblio
(defun weblio (str)
  "Search with Weblio by STR."
  (interactive (list
                (region-or-read-string "Weblio: ")))
  (eww-browse-url (format "https://www.weblio.jp/content/%s"
                      (upcase (url-hexify-string str)))))
;;; wikipedia
(defun wikipedia (str)
  "Search with Wikipedia by STR."
  (interactive (list
                (region-or-read-string "Wikipedia: ")))
  (eww-browse-url (format "https://ja.wikipedia.org/wiki/%s"
                      (upcase (url-hexify-string str)))))

;; ------------------------------------------------------------------------
;; helm (google)

(global-set-key (kbd "M-g s") 'helm-google)
(global-set-key (kbd "M-g a") 'helm-google-suggest)

;; ------------------------------------------------------------------------
;; howdoi

(when (require 'howdoi nil t)
  (setq howdoi-display-question t)
  (defun howdoi-show-url (&rest ignore)
    (interactive)
    (message "%s" howdoi-current-stackoverflow-url))
  (advice-add 'howdoi-pop-answer-to-buffer-callback :after 'howdoi-show-url)
  (define-key howdoi-mode-map (kbd "c") 'howdoi-show-url)
  )

;; ------------------------------------------------------------------------
;; yagist

(setq yagist-github-token (my-lisp-load "yagist-github-token"))
;; (require 'yagist)
(autoload 'yagist "yagist" nil t)

;; ------------------------------------------------------------------------

(provide 'cnf-webservice.el)
;;; cnf-webservice.el ends here
