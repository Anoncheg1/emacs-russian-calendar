;;; russian-calendar-2024.el --- Holidays and conferences -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.3
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; 2024

;;; Code:
(require 'calendar)
(require 'holidays)

(defvar russian-calendar-2024-holidays
  (mapcar 'purecopy
          '(
            (holiday-fixed 1 1 "Вых. Новогодние каникулы")
            (holiday-fixed 1 2 "Вых. Новогодние каникулы")
            (holiday-fixed 1 3 "Вых. Новогодние каникулы")
            (holiday-fixed 1 4 "Вых. Новогодние каникулы")
            (holiday-fixed 1 5 "Вых. Новогодние каникулы")
            (holiday-fixed 1 8 "Вых. Новогодние каникулы")
            (holiday-fixed 1 7 "Вых. Рождество Христово")
            (holiday-fixed 2 22 "* Сокращённый день")
            (holiday-fixed 2 23 "Вых. День защитника Отечества")
            (holiday-fixed 3 7 "* Сокращённый день")
            (holiday-fixed 3 8 "Вых. Международный женский день")
            (holiday-fixed 4 27 "Рабочий день")
            (holiday-fixed 4 29 "Вых. May holiday")
            (holiday-fixed 4 30 "Вых. May holiday")
            (holiday-fixed 5 1 "Вых. Праздник Весны и Труда")
            (holiday-fixed 5 8 "* Сокращённый день")
            (holiday-fixed 5 9 "Вых. День Победы")
            (holiday-fixed 5 10 "Вых. Выходные")
            (holiday-fixed 6 11 "* Сокращённый день")
            (holiday-fixed 6 12 "Вых. День России")
            (holiday-fixed 11 4 "Вых. День народного единства")
            (holiday-fixed 11 2 "* Сокращённый день")
            (holiday-fixed 12 28 "Рабочий день")
            (holiday-fixed 12 30 "Вых. Выходной")
            (holiday-fixed 12 31 "Вых. Выходной")))
  "Russian calendar holidays.")

(defvar russian-calendar-2024-general-holidays
  (mapcar 'purecopy
          '(
            (holiday-fixed 2 14 "Valentine's Day")
            (holiday-fixed 4 1 "April Fools' Day")
            (holiday-fixed 10 31 "Halloween")))
  "International holidays.")

(defvar russian-calendar-2024-open-source-confs
  (mapcar 'purecopy
          '(
            (holiday-fixed 2 3 "FOSDEM https://fosdem.org")
            (holiday-fixed 12 7 "EmacsConf https://emacsconf.org/")))
  "Open Source conferences.")

(defvar russian-calendar-2024-ai-confs
  (mapcar 'purecopy
          '(
            (holiday-fixed 12 10 "NeurIPS https://neurips.cc/")))
  "AI conferences.")

(defvar russian-calendar-2024-russian-it-confs
  (mapcar 'purecopy
          '(
          (holiday-fixed 12 11 "AI Journey https://aij.ru/")
          (holiday-fixed 11 28 "TAdviser SummIT https://tadvisersummit.ru/")))
  "Russian IT and AI Conferences.")

(defvar russian-calendar-2024-old-slavic-fests
  (mapcar 'purecopy
          '(
            (holiday-fixed 12 21 "Карачун (смерть)")  ;; с 22 на 23 декабря 2024:21 2025:21 2026:21 2027:22 2028:21 2029:21
            (holiday-fixed 1 6 "Коляда по 19 января")
            (holiday-fixed 3 20 "Весеннее равноденствие") ;; 2024-2029:20
            (holiday-greek-orthodox-easter -55 "Масленица (неделя)") ;; в течение недели перед Великим постом
            (holiday-fixed 4 22 "Красная горка или Лельник")
            (holiday-fixed 6 20 "Летнее солнцестояние") ;; 20 июня 2024:20 2025-2027:21 2028:20 2029:21
            (holiday-fixed 7 6 "Ночь Ивана Купалы") ;; с 6 на 7 июля
            (holiday-fixed 8 2 "Перунов день") ;; 2 августа
            (holiday-fixed 9 22 "Осеннее равноденствие") ;; 2024:22 2025:22 2026:23 2027:23 2028:22 2029:22
            (holiday-fixed 10 31 "Велесова ночь") ;; с 31 октября на 1 ноября
            ))
  "Old slavic folk holidays.")

(provide 'russian-calendar-2024)
;;; russian-calendar-2024.el ends here
