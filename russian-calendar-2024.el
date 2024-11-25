;;; russian-calendar-2024.el --- Holidays and conferences -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.2
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;

;;; Code:

(require 'holidays)

(defvaralias 'russian-calendar-holidays 'russian-calendar-2024-holidays)
(defvaralias 'russian-calendar-general-holidays 'russian-calendar-2024-general-holidays)
(defvaralias 'russian-calendar-open-source-confs 'russian-calendar-2024-open-source-confs)
(defvaralias 'russian-calendar-ai-confs 'russian-calendar-2024-ai-confs)


(defvar russian-calendar-holidays
  '(
    (holiday-fixed 1 1 "Новогодние каникулы")
    (holiday-fixed 1 2 "Новогодние каникулы")
    (holiday-fixed 1 3 "Новогодние каникулы")
    (holiday-fixed 1 4 "Новогодние каникулы")
    (holiday-fixed 1 5 "Новогодние каникулы")
    (holiday-fixed 1 8 "Новогодние каникулы")
    (holiday-fixed 1 7 "Рождество Христово")
    (holiday-fixed 2 22 "* Сокращённый день")
    (holiday-fixed 2 23 "День защитника Отечества")
    (holiday-fixed 3 7 "* Сокращённый день")
    (holiday-fixed 3 8 "Международный женский день")
    (holiday-fixed 4 27 "Work day")
    (holiday-fixed 4 29 "May holiday")
    (holiday-fixed 4 30 "May holiday")
    (holiday-fixed 5 1 "Праздник Весны и Труда")
    (holiday-fixed 5 8 "* Сокращённый день")
    (holiday-fixed 5 9 "День Победы")
    (holiday-fixed 5 10 "Выходные")
    (holiday-fixed 6 11 "* Сокращённый день")
    (holiday-fixed 6 12 "День России")
    (holiday-fixed 11 4 "День народного единства")
    (holiday-fixed 11 2 "* Сокращённый день")
    (holiday-fixed 12 28 "Work day")
    (holiday-fixed 12 30 "Weekend day")
    (holiday-fixed 12 31 "Weekend day")))

(defvar russian-calendar-general-holidays
  '(
    (holiday-fixed 2 14 "Valentine's Day")
    (holiday-fixed 4 1 "April Fools' Day")
    (holiday-fixed 10 31 "Halloween")))

(defvar russian-calendar-open-source-confs
  '(
    (holiday-fixed 2 3 "FOSDEM https://fosdem.org")
    (holiday-fixed 12 7 "EmacsConf https://emacsconf.org/")))

(defvar russian-calendar-ai-confs
  '(
    (holiday-fixed 12 10 "NeurIPS https://neurips.cc/")))

(defvar russian-calendar-russian-it-confs
  '(
  (holiday-fixed 12 11 "AI Journey https://aij.ru/")
  (holiday-fixed 11 28 "TAdviser SummIT https://tadvisersummit.ru/")))

(provide 'russian-calendar-2024)
;;; russian-calendar-2024.el ends here
