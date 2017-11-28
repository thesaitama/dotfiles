
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

(defun open-mac (path)
  (start-process "dired-open-mac" nil "open" path))

;; ------------------------------------------------------------------------
;; mac quick-lock

(defun quicklook-file (path)
  (interactive)
  (defvar cur nil)
  (defvar old nil)
  (setq old cur)
  (setq cur (start-process "ql-file" nil "qlmanage" "-p" path))
  (when old (delete-process old)))

(defun my-dired-open ()
  (interactive)
  (let ((exts-ql   '("jpeg" "jpg" "png" "gif" "tiff"))
        (exts-open '("avi" "mkv" "mp4" "mts" "psd" "ai" "pdf")))
     (cond ((file-accessible-directory-p (dired-get-file-for-visit))
            (call-interactively 'dired-find-alternate-file))
           ((member (downcase (file-name-extension (dired-get-file-for-visit))) exts-ql)
            (funcall 'quicklook-file (dired-get-file-for-visit)))
           ((member (downcase (file-name-extension (dired-get-file-for-visit))) exts-open)
            (funcall 'open-mac (dired-get-file-for-visit)))
           (t
            (call-interactively 'dired-find-file-other-window)))))
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "o" 'my-dired-open)))

;; ------------------------------------------------------------------------
;; osx-dictionary

;; Support Chinese word
;;(setq osx-dictionary-use-chinese-text-segmentation t)

;; Key bindings
;;(global-set-key (kbd "C-c d") 'osx-dictionary-search-word-at-point)
;;(global-set-key (kbd "C-c i") 'osx-dictionary-search-input)


