;;; .emacs.el --- thesaitama Emacs configuration

;;  _   _                     _ _
;; | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
;; | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
;; | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
;;  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

;;; Commentary:
;;
;; thesaitama@ .emacs.el
;; Last Update: 2018-12-26 12:39:01
;; tested with: Emacs 26.1 (or 22.1), macOS 10.14, Windows 10

;; install
;; > sudo apt-get install libncurses5-dev libgnutls-openssl27 libgnutls28-dev
;; > wget http://ftpmirror.gnu.org/emacs/emacs-26.1.tar.gz
;; > tar xzvf emacs-26.1.tar.gz
;; > cd emacs-26.1
;; > ./configure --without-x
;; > make
;; > sudo make install

;;; Code:

;; ------------------------------------------------------------------------
;; utility function

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

(provide '.emacs.el)
;;; .emacs.el ends here


