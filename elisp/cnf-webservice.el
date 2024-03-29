;;; cnf-webservice.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; newsticker

(setq-default newsticker-retrieval-interval 0)
(setq-default newsticker-url-list
              '(("Yahoo" "https://news.yahoo.co.jp/pickup/rss.xml")
                ("NHK" "https://www3.nhk.or.jp/rss/news/cat0.xml")
                ("Sankei" "https://www.sankeibiz.jp/rss/news/flash.xml")
                ))
;; (setq-default newsticker-url-list-defaults
;;               '(("" "https://www.sankeibiz.jp/rss/news/flash.xml")))
(set-variable 'newsticker-html-renderer #'shr-render-region)

;; ------------------------------------------------------------------------
;; xah-lookup

(set-variable 'xah-lookup-browser-function 'eww)

;; ------------------------------------------------------------------------
;; [google] google-translate

(when (require 'google-translate nil t)

  ;; Quick fix 27.1
  ;; https://github.com/atykhonov/google-translate/issues/52
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  ;; https://solist.work/blog/posts/google-translate/
  (defun chromium-translate ()
    "Open google translate with chromium."
    (interactive)
    (if (use-region-p)
        (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
	      (deactivate-mark)
	      (if (string-match (format "\\`[%s]+\\'" "[:ascii:]") string)
	          (browse-url (concat "https://translate.google.com/?source=gtx#en/ja/"
				                  (url-hexify-string string)))
	        (browse-url (concat "https://translate.google.com/?source=gtx#ja/en/"
			                    (url-hexify-string string)))))
      (let ((string (read-string "Google Translate: ")))
        (if (string-match
	         (format "\\`[%s]+\\'" "[:ascii:]") string)
	        (browse-url
	         (concat "https://translate.google.com/?source=gtx#en/ja/" (url-hexify-string string)))
	      (browse-url
	       (concat "https://translate.google.com/?source=gtx#ja/en/" (url-hexify-string string))))))
    )
  (defvar toggle-translate-flg nil "Toggle Google Translate flag.")
  (defun toggle-translate ()
    "Toggle translate function."
    (interactive)
    (if toggle-translate-flg
        (progn
          (global-set-key (kbd "M-g t") 'google-translate-enja-or-jaen)
	      (setq toggle-translate-flg nil))
      (progn
        (global-set-key (kbd "M-g t") 'chromium-translate)
        (setq toggle-translate-flg t))))

  (set-variable 'google-translate-default-source-language "ja")
  (set-variable 'google-translate-default-target-language "en")
  (set-variable 'google-translate-base-url "https://translate.google.com/translate_a/single")
  (set-variable 'google-translate--tkk-url "https://translate.google.com/")
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
;; [google] helm-google

(global-set-key (kbd "M-g s") 'helm-google)
(global-set-key (kbd "M-g a") 'helm-google-suggest)

;; ------------------------------------------------------------------------
;; [google] google-this

(global-set-key (kbd "M-g o") 'google-this)
(setq google-this-location-suffix "co.jp")

(defun google-this-url ()
  "URL for google search."
  ;; search within 5 years
  (concat google-this-base-url google-this-location-suffix
          "/search?q=%s&as_qdr=y5&lr=lang_ja"))

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

;; Weblio
(defun weblio (str)
  "Search with Weblio with STR."
  (interactive (list
                (region-or-read-string "Weblio: ")))
  (eww-browse-url (format "https://www.weblio.jp/content/%s"
                      (upcase (url-hexify-string str)))))
;; Wikipedia
(defun wikipedia (str)
  "Search with Wikipedia with STR."
  (interactive (list
                (region-or-read-string "Wikipedia: ")))
  (eww-browse-url (format "https://ja.wikipedia.org/wiki/%s"
                          (url-hexify-string str))))

;; Wiktionary
(defun wiktionary (str)
  "Search with Wiktionary with STR."
  (interactive (list
                (region-or-read-string "Wiktionary: ")))
  (eww-browse-url (format "https://en.wiktionary.org/wiki/%s"
                      (url-hexify-string str))))

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
