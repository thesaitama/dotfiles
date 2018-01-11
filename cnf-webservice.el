;;; cnf-webservice.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitam Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; google-translate

(require 'google-translate)

;;(custom-set-variables
;;  '(google-translate-default-source-language "ja")
;;  '(google-translate-default-target-language "en"))

(setq google-translate-default-source-language "ja")
(setq google-translate-default-target-language "en")

(defvar google-translate-english-chars "[:ascii:]’“”–"
  "English characters list.")
(defun google-translate-enja-or-jaen (&optional string)
  "Auto detection translate language by region or current sentence."
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
;; howdoi

(require 'howdoi)
(setq howdoi-display-question t)
(defun howdoi-show-url (&rest ignore)
  (interactive)
  (message "%s" howdoi-current-stackoverflow-url))
(advice-add 'howdoi-pop-answer-to-buffer-callback :after 'howdoi-show-url)
(define-key howdoi-mode-map (kbd "c") 'howdoi-show-url)

;; ------------------------------------------------------------------------
;; yagist

(setq yagist-github-token (my-lisp-load "yagist-github-token"))
(require 'yagist)

;; ------------------------------------------------------------------------

(provide 'cnf-webservice.el)
;;; cnf-webservice.el ends here
