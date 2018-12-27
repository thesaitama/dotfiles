;;; cnf-calendar.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; calendar

(eval-after-load 'calendar
  '(progn
     (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

     (when (require 'japanese-holidays nil t)
       (setq calendar-holidays
             (append japanese-holidays
                     holiday-local-holidays holiday-other-holidays))
       (setq calendar-mark-holidays-flag t)
       (setq mark-holidays-in-calendar t)
       (setq japanese-holiday-weekend-marker
             '(holiday nil nil nil nil nil japanese-holiday-saturday))
       (setq japanese-holiday-weekend '(0 6))
       (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
       (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
       (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
       )
     )
  )

;; ------------------------------------------------------------------------

(provide 'cnf-calendar.el)
;;; cnf-calendar.el ends here
