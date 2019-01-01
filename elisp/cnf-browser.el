;;; cnf-browser.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; w3m

(defvar w3m-command "w3m")
(defvar w3m-search-default-engine "google")
(defvar w3m-home-page "http://www.google.co.jp")
(defvar w3m-use-cookies t)
(eval-after-load "w3m-search"
  '(add-to-list 'w3m-search-engine-alist
  '("google" "https://encrypted.google.com/search?num=100&ie=utf-8&oe=utf-8&hl=ja&safe=off&filter=0&pws=0&complete=0&gbv=1&q=%s" utf-8)))

;; ------------------------------------------------------------------------

(provide 'cnf-browser.el)
;;; cnf-browser.el ends here


