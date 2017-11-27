
;; macOSX

;; ------------------------------------------------------------------------
;; ucs normalize

(require 'ucs-normalize)
(set-file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; ------------------------------------------------------------------------
;; clipboard

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; ------------------------------------------------------------------------
;; osx-dictionary

;; Support Chinese word
;;(setq osx-dictionary-use-chinese-text-segmentation t)

;; Key bindings
;;(global-set-key (kbd "C-c d") 'osx-dictionary-search-word-at-point)
;;(global-set-key (kbd "C-c i") 'osx-dictionary-search-input)


