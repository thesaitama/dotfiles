
;; ------------------------------------------------------------------------
;; w3m

(require 'w3m)
(setq w3m-command "w3m")
(setq exec-path (cons "/opt/local/bin/w3m" exec-path))
(eval-after-load "w3m-search"
  '(add-to-list 'w3m-search-engine-alist
  '("google" "https://encrypted.google.com/search?num=100&ie=utf-8&oe=utf-8&hl=ja&safe=off&filter=0&pws=0&complete=0&gbv=1&q=%s" utf-8)))
(setq w3m-search-default-engine "google")
(setq w3m-home-page "http://www.google.co.jp")
(setq w3m-use-cookies t)

(provide 'cnf-browser.el)
;;; cnf-browser.el ends here


