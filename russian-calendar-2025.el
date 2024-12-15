;;; russian-calendar-2025.el --- Holidays and conferences  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.5
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;

;;; Code:
(require 'calendar)
(require 'holidays)

(defun russian-calendar-holiday-fixed-2025 (month day string)
  "Holiday fixed-limited for 2024 year only.
Arguments MONTH, DAY, STRING is standard arguments for
`holiday-fixed' function.  Return formatted date or nil."
  (if (= (bound-and-true-p displayed-year) 2025)
      (holiday-fixed month day string)))

(defalias 'holidayf-2025 'russian-calendar-holiday-fixed-2025)

(defvar russian-calendar-2025-holidays
  (mapcar 'purecopy
          '(
          (holiday-fixed 1 1 "Вых. Новогодние каникулы")
          (holiday-fixed 1 2 "Вых. Новогодние каникулы")
          (holiday-fixed 1 3 "Вых. Новогодние каникулы")
          (holiday-fixed 1 4 "Вых. Новогодние каникулы")
          (holiday-fixed 1 5 "Вых. Новогодние каникулы")
          (holiday-fixed 1 8 "Вых. Новогодние каникулы")
          (holiday-fixed 1 7 "Вых. Рождество Христово")
          (holiday-fixed 2 23 "Вых. День защитника Отечества")
          (holidayf-2025 3 7 "* Сокращённый день перед Международный женский день")
          (holiday-fixed 3 8 "Вых. Международный женский день")
          (holidayf-2025 4 30 "* Сокращённый день перед Праздник Весны и Труда")
          (holiday-fixed 5 1 "Вых. Праздник Весны и Труда")
          (holidayf-2025 5 2 "Вых. после Праздник Весны и Труда")
          (holidayf-2025 5 8 "Вых. перед День Победы")
          (holiday-fixed 5 9 "Вых. День Победы")
          (holidayf-2025 6 11 "* Сокращённый день День России")
          (holiday-fixed 6 12 "Вых. День России")
          (holidayf-2025 6 13 "Вых. после День России")
          (holidayf-2025 11 1 "! Рабочий день за 3")
          (holidayf-2025 11 3 "Вых. перед День народного единства")
          (holiday-fixed 11 4 "Вых. День народного единства")
          (holiday-fixed 12 31 "Вых. перед Новый год")))
  "Russian calendar holidays.")

(defvar russian-calendar-2025-open-source-confs
  (mapcar 'purecopy
          '(
            (holidayf-2025 2 1 "FOSDEM https://fosdem.org")
            (holidayf-2025 12 7 "EmacsConf https://emacsconf.org/ (will change)")
            ))
  "Open Source conferences.")

(defvar russian-calendar-2025-ai-confs
  (mapcar 'purecopy
          '(
            (holidayf-2025 10 22 "PyTorch")
            (holidayf-2025 5 5 "IEEE CAI https://ieeecai.org/")
            (holidayf-2025 2 13 "WAIC https://www.worldaicannes.com")
            (holidayf-2025 12 10 "NeurIPS https://neurips.cc/ (will change)")))
  "AI conferences.")

(defvar russian-calendar-2025-russian-it-confs
  (mapcar 'purecopy
          '(
            (holidayf-2025 12 11 "AI Journey https://aij.ru/ (will change)")
            (holidayf-2025 11 28 "TAdviser SummIT https://tadvisersummit.ru/ (will change)")))
  "Russian IT and AI Conferences.")

(defvar russian-calendar-2025-old-slavic-fests
  (mapcar 'purecopy
          '(
            (holidayf-2025 12 21 "Карачун (смерть)")  ;; с 22 на 23 декабря 2024:21 2025:21 2026:21 2027:22 2028:21 2029:21
            (holiday-fixed 1 6 "Коляда по 19 января")
            (holidayf-2025 3 20 "Весеннее равноденствие") ;; 2024-2029:20
            (holiday-greek-orthodox-easter -55 "Масленица (неделя)") ;; в течение недели перед Великим постом
            (holiday-greek-orthodox-easter 7 "Красная горка или Лельник, поминовение усопших")
            (holidayf-2025 6 21 "Летнее солнцестояние") ;; 20 июня 2024:20 2025-2027:21 2028:20 2029:21
            (holiday-fixed 7 6 "Ночь Ивана Купалы") ;; с 6 на 7 июля
            (holiday-fixed 8 2 "Перунов день") ;; 2 августа
            (holidayf-2025 9 22 "Осеннее равноденствие") ;; 2024:22 2025:22 2026:23 2027:23 2028:22 2029:22
            (holiday-fixed 10 31 "Велесова ночь") ;; с 31 октября на 1 ноября
            ))
  "Old slavic folk holidays.")

(provide 'russian-calendar-2025)
;;; russian-calendar-2025.el ends here
