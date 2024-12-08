;;; russian-calendar-2025.el --- Holidays and conferences  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.3
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;

;;; Code:
(require 'calendar)
(require 'holidays)

;; - Check that we are are at right year.
(let ((cyear (number-to-string
              ; get current year
              (nth 5 (decode-time (current-time))))))
  (if (not (string-equal cyear "2025"))
      (message
       (concat "Warning: package russian-calendar-2025 is obsolate."))))

(defvaralias 'russian-calendar-holidays
 'russian-calendar-2025-holidays)
(defvaralias 'russian-calendar-general-holidays
 'russian-calendar-2025-general-holidays)
(defvaralias 'russian-calendar-open-source-confs
 'russian-calendar-2025-open-source-confs)
(defvaralias 'russian-calendar-ai-confs
 'russian-calendar-2025-ai-confs)
(defvaralias russian-calendar-russian-it-confs
 'russian-calendar-2025-russian-it-confs)
(defvaralias 'russian-calendar-old-slavic-fests
 russian-calendar-2025-old-slavic-fests)

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
          (holiday-fixed 3 7 "* Сокращённый день перед Международный женский день")
          (holiday-fixed 3 8 "Вых. Международный женский день")
          (holiday-fixed 4 30 "* Сокращённый день перед Праздник Весны и Труда")
          (holiday-fixed 5 1 "Вых. Праздник Весны и Труда")
          (holiday-fixed 5 2 "Вых. после Праздник Весны и Труда")
          (holiday-fixed 5 8 "Вых. перед День Победы")
          (holiday-fixed 5 9 "Вых. День Победы")
          (holiday-fixed 6 11 "* Сокращённый день День России")
          (holiday-fixed 6 12 "Вых. День России")
          (holiday-fixed 6 13 "Вых. после День России")
          (holiday-fixed 11 1 "! Рабочий день за 3")
          (holiday-fixed 11 3 "Вых. перед День народного единства")
          (holiday-fixed 11 4 "Вых. День народного единства")
          (holiday-fixed 12 31 "Вых. перед Новый год")))
  "Russian calendar holidays.")

(defvar russian-calendar-2025-general-holidays
  (mapcar 'purecopy
          '(
            (holiday-fixed 2 14 "Valentine's Day")
            (holiday-fixed 4 1 "April Fools' Day")
            (holiday-fixed 10 31 "Halloween")))
  "International holidays.")

(defvar russian-calendar-2025-open-source-confs
  (mapcar 'purecopy
          '(
            (holiday-fixed 2 1 "FOSDEM https://fosdem.org")
            (holiday-fixed 12 7 "EmacsConf https://emacsconf.org/") ; will change
            ))
  "Open Source conferences.")

(defvar russian-calendar-2025-ai-confs
  (mapcar 'purecopy
          '(
            (holiday-fixed 10 22 "PyTorch")
            (holiday-fixed 5 5 "IEEE CAI https://ieeecai.org/")
            (holiday-fixed 2 13 "WAIC https://www.worldaicannes.com")
            (holiday-fixed 12 10 "NeurIPS https://neurips.cc/") ; will change
            ))
  "AI conferences.")

(defvar russian-calendar-2025-russian-it-confs
  (mapcar 'purecopy
          '(
          (holiday-fixed 12 11 "AI Journey https://aij.ru/") ; will change
          (holiday-fixed 11 28 "TAdviser SummIT https://tadvisersummit.ru/") ; will change
          ))
  "Russian IT and AI Conferences.")

(defvar russian-calendar-2025-old-slavic-fests
  (mapcar 'purecopy
          '(
            (holiday-fixed 12 21 "Карачун (смерть)")  ;; с 22 на 23 декабря 2024:21 2025:21 2026:21 2027:22 2028:21 2029:21
            (holiday-fixed 1 6 "Коляда по 19 января")
            (holiday-fixed 3 20 "Весеннее равноденствие") ;; 2024-2029:20
            (holiday-greek-orthodox-easter -55 "Масленица (неделя)") ;; в течение недели перед Великим постом
            (holiday-fixed 4 22 "Красная горка или Лельник")
            (holiday-fixed 6 21 "Летнее солнцестояние") ;; 20 июня 2024:20 2025-2027:21 2028:20 2029:21
            (holiday-fixed 7 6 "Ночь Ивана Купалы") ;; с 6 на 7 июля
            (holiday-fixed 8 2 "Перунов день") ;; 2 августа
            (holiday-fixed 9 22 "Осеннее равноденствие") ;; 2024:22 2025:22 2026:23 2027:23 2028:22 2029:22
            (holiday-fixed 10 31 "Велесова ночь") ;; с 31 октября на 1 ноября
            ))
  "Old slavic folk holidays.")

(provide 'russian-calendar-2025)

;;; russian-calendar-2025.el ends here
