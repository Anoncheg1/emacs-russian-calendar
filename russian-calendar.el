;;; russian-calendar.el --- Russian holidays and conferences. Updated 2024-11-22  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg

;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Include:
;; - Russian holidays
;; - International holidays: Valentine's Day, April Fools' Day, Halloween
;; - Open source conferences: Emacs, FSF, GNU, FOSDEM
;; - AI conferences: PyTorh, NeurIPS, IEEE CAI, WAIC, AI Journey dec
;;   + TAdviser SummIT nov + CNews Forum nov

;; Usage:
;; (require 'calendar)
;; (require 'holidays)
;; (require 'russian-calendar-2024)
;; (setopt diary-show-holidays-flag t)
;; (setopt calendar-mark-holidays-flag t)
;; (setopt calendar-week-start-day 1)
;; (setopt calendar-holidays (append russian-calendar-holidays
;;                                   russian-calendar-general-holidays
;;                                   russian-calendar-open-source-confs
;;                                   russian-calendar-ai-confs))

;;; Code:

(require 'russian-calendar-2024)

;;; Code:

(provide 'russian-calendar)
;;; russian-calendar.el ends here
