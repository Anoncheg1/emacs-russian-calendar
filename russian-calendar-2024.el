;;; russian-calendar-2024.el --- Holidays and conferences -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.7

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>

;;; Commentary:
;; 2024

;;; Code:
(require 'calendar)
(require 'holidays)

(defun russian-calendar-2024-h (month day string)
  "Holiday fixed-limited for 2024 year only.
Arguments MONTH, DAY, STRING is standard arguments for
`holiday-fixed' function.  Return formatted date or nil."
  (if (= (bound-and-true-p displayed-year) 2024)
      (holiday-fixed month day string)))

;; (defalias 'holidayf-2024 'russian-calendar-2024-holiday-fixed)

(defvar russian-calendar-2024-holidays
  (mapcar #'purecopy
          '(
            (holiday-fixed 1 1 "Вых. Новогодние каникулы")
            (holiday-fixed 1 2 "Вых. Новогодние каникулы")
            (holiday-fixed 1 3 "Вых. Новогодние каникулы")
            (holiday-fixed 1 4 "Вых. Новогодние каникулы")
            (holiday-fixed 1 5 "Вых. Новогодние каникулы")
            (holiday-fixed 1 8 "Вых. Новогодние каникулы")
            (holiday-fixed 1 7 "Вых. Рождество Христово")
            (russian-calendar-2024-h 2 22 "* Сокращённый день")
            (holiday-fixed 2 23 "Вых. День защитника Отечества")
            (russian-calendar-2024-h 3 7 "* Сокращённый день перед Международный женский день")
            (holiday-fixed 3 8 "Вых. Международный женский день")
            (russian-calendar-2024-h 4 27 "Рабочий день")
            (russian-calendar-2024-h 4 29 "Вых. May holiday")
            (russian-calendar-2024-h 4 30 "Вых. May holiday")
            (holiday-fixed 5 1 "Вых. Праздник Весны и Труда")
            (russian-calendar-2024-h 5 8 "* Сокращённый день")
            (holiday-fixed 5 9 "Вых. День Победы")
            (russian-calendar-2024-h 5 10 "Вых. Выходной")
            (russian-calendar-2024-h 6 11 "* Сокращённый день")
            (russian-calendar-2024-h 6 12 "Вых. День России")
            (holiday-fixed 11 4 "Вых. День народного единства")
            (russian-calendar-2024-h 11 2 "* Сокращённый день")
            (russian-calendar-2024-h 12 28 "Рабочий день")
            (russian-calendar-2024-h 12 30 "Вых. Выходной")
            (holiday-fixed 12 31 "Вых. перед Новый год")))
  "Russian calendar holidays.")

(defvar russian-calendar-2024-open-source-confs
  (mapcar #'purecopy
          '(
            (russian-calendar-2024-h 2 3 "FOSDEM https://fosdem.org")
            (russian-calendar-2024-h 12 7 "EmacsConf https://emacsconf.org/")))
  "Open Source conferences.")

(defvar russian-calendar-2024-ai-confs
  (mapcar #'purecopy
          '(
            (russian-calendar-2024-h 12 10 "NeurIPS https://neurips.cc/")))
  "AI conferences.")

(defvar russian-calendar-2024-russian-it-confs
  (mapcar #'purecopy
          '(
            (russian-calendar-2024-h 12 11 "AI Journey https://aij.ru/")
            (russian-calendar-2024-h 11 28 "TAdviser SummIT https://tadvisersummit.ru/")))
  "Russian IT and AI Conferences.")

(defvar russian-calendar-2024-old-slavic-fests
  (mapcar #'purecopy
          '(
            (russian-calendar-2024-h 12 21 "Карачун (смерть)")  ;; с 22 на 23 декабря 2024:21 2025:21 2026:21 2027:22 2028:21 2029:21
            (holiday-fixed 1 6 "Коляда по 19 января")
            (russian-calendar-2024-h 3 20 "Весеннее равноденствие") ;; 2024-2029:20
            (holiday-greek-orthodox-easter -55 "Масленица (неделя)") ;; в течение недели перед Великим постом
            (holiday-greek-orthodox-easter 7 "Красная горка или Лельник, поминовение усопших")
            (russian-calendar-2024-h 6 20 "Летнее солнцестояние") ;; 20 июня 2024:20 2025-2027:21 2028:20 2029:21
            (holiday-fixed 7 6 "Ночь Ивана Купалы") ;; с 6 на 7 июля
            (holiday-fixed 8 2 "Перунов день") ;; 2 августа
            (russian-calendar-2024-h 9 22 "Осеннее равноденствие") ;; 2024:22 2025:22 2026:23 2027:23 2028:22 2029:22
            (holiday-fixed 10 31 "Велесова ночь") ;; с 31 октября на 1 ноября
            ))
  "Old slavic folk holidays.")

(provide 'russian-calendar-2024)
;;; russian-calendar-2024.el ends here
