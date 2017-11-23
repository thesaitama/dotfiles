
;; ------------------------------------------------------------------------
;; google-translate

(require 'google-translate)
(custom-set-variables
  '(google-translate-default-source-language "ja")
  '(google-translate-default-target-language "en"))

(defvar google-translate-english-chars "[:ascii:]’“”–"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
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

;; ------------------------------------------------------------------------
;; helm (google)

(global-set-key (kbd "M-g s") 'helm-google)
(global-set-key (kbd "M-g a") 'helm-google-suggest)

;; ------------------------------------------------------------------------
;; helm-qiita

;;(setq helm-qiita-username "Your Qiita Account")
;;(setq helm-qiita-organization "Your Organization") ;; optional.
;;(setq helm-qiita-access-token "Your Access Token") ;; See https://qiita.com/settings/applications
(setq helm-qiita-username (my-lisp-load "helm-qiita-username"))
(setq helm-qiita-access-token (my-lisp-load "helm-qiita-access-token"))
(helm-qiita-initialize)

;; ------------------------------------------------------------------------
;; yagist

;(setq yagist-github-token "******************************")
(setq yagist-github-token (my-lisp-load "yagist-github-token"))
(require 'yagist)


