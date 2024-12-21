;;; russian-calendar-2025.el --- Holidays and conferences  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.8

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
;;

;;; Code:
(require 'calendar)
(require 'holidays)

(defun russian-calendar-2025-h (month day string)
  "Holiday fixed-limited for 2024 year only.
Arguments MONTH, DAY, STRING is standard arguments for
`holiday-fixed' function.  Return formatted date or nil."
  (if (= (bound-and-true-p displayed-year) 2025)
      (holiday-fixed month day string)))

;; (defalias 'holidayf-2025 'russian-calendar-holiday-fixed-2025)

(defvar russian-calendar-2025-holidays
  (mapcar #'purecopy
          '(
          (holiday-fixed 1 1 "Вых. Новогодние каникулы")
          (holiday-fixed 1 2 "Вых. Новогодние каникулы")
          (holiday-fixed 1 3 "Вых. Новогодние каникулы")
          (holiday-fixed 1 4 "Вых. Новогодние каникулы")
          (holiday-fixed 1 5 "Вых. Новогодние каникулы")
          (holiday-fixed 1 8 "Вых. Новогодние каникулы")
          (holiday-fixed 1 7 "Вых. Рождество Христово")
          (holiday-fixed 2 23 "Вых. День защитника Отечества")
          (russian-calendar-2025-h 3 7 "* Сокращённый день перед Международный женский день")
          (holiday-fixed 3 8 "Вых. Международный женский день")
          (russian-calendar-2025-h 4 30 "* Сокращённый день перед Праздник Весны и Труда")
          (holiday-fixed 5 1 "Вых. Праздник Весны и Труда")
          (russian-calendar-2025-h 5 2 "Вых. после Праздник Весны и Труда")
          (russian-calendar-2025-h 5 8 "Вых. перед День Победы")
          (holiday-fixed 5 9 "Вых. День Победы")
          (russian-calendar-2025-h 6 11 "* Сокращённый день День России")
          (holiday-fixed 6 12 "Вых. День России")
          (russian-calendar-2025-h 6 13 "Вых. после День России")
          (russian-calendar-2025-h 11 1 "! Рабочий день за 3")
          (russian-calendar-2025-h 11 3 "Вых. перед День народного единства")
          (holiday-fixed 11 4 "Вых. День народного единства")
          (holiday-fixed 12 31 "Вых. перед Новый год")))
  "Russian calendar holidays.")

(defvar russian-calendar-2025-open-source-confs
  (mapcar #'purecopy
          '(
            (russian-calendar-2025-h 2 1  "FOSDEM https://fosdem.org")
            (russian-calendar-2025-h 12 7 "EmacsConf https://emacsconf.org/ (will change)")
            (if russian-calendar-all-open-source-confs-flag
                (append
                  (russian-calendar-2025-h 1 20 "Australia. Open technologies. https://2025.everythingopen.au/")
                  (russian-calendar-2025-h 3 6 "Australia. SCALE 22x https://www.socallinuxexpo.org/scale/22x")
                  (russian-calendar-2025-h 3 9 "Southern California Linux Expo. SCALE 22x https://www.socallinuxexpo.org/scale/22x")
                  (russian-calendar-2025-h 3 10 "FOSS Backstage Netdev 0x19 https://25.foss-backstage.de/")
                  (russian-calendar-2025-h 3 18 "Nordic PGDay pgDay Paris https://2025.nordicpgday.org/, LF Member Summit https://events.linuxfoundation.org/lf-member-summit/")
                  (russian-calendar-2025-h 3 20 "pgDay Paris https://2025.pgday.paris/")
                  (russian-calendar-2025-h 3 20 "LSFMM https://events.linuxfoundation.org/lsfmmbpf/")
                  (russian-calendar-2025-h 4 9 "sambaXP 2025 https://sambaxp.org/")
                  (russian-calendar-2025-h 4 14 "foss-north https://foss-north.se/2025/")
                  (russian-calendar-2025-h 4 26 "LIT Augsburg https://www.luga.de/LIT-2025/")
                  (russian-calendar-2025-h 4 29 "stackconf https://stackconf.eu/")
                  (russian-calendar-2025-h 5 8 "PGConf.de https://2025.pgconf.de/")
                  (russian-calendar-2025-h 5 13 "PGConf.dev https://2025.pgconf.dev/")
                  (russian-calendar-2025-h 5 16 "PyCon US (Pittsburgh, Pennsylvania) https://us.pycon.org/2025/")
                  (russian-calendar-2025-h 5 22 "NLUUG Spring Conference 2025 https://nluug.nl/")
                  (russian-calendar-2025-h 6 23 "OSSNA https://events.linuxfoundation.org/open-source-summit-north-america/")
                  (russian-calendar-2025-h 6 26 "LSSNA https://events.linuxfoundation.org/linux-security-summit-north-america/, LAC https://jimlac25.inria.fr/lac/")
                  (russian-calendar-2025-h 6 26 "LAC https://jimlac25.inria.fr/lac/")
                  (russian-calendar-2025-h 7 31 "FOSSY https://2025.fossy.us/")
                  (russian-calendar-2025-h 8 5 "OSS India https://events.linuxfoundation.org/open-source-summit-india/")
                  (russian-calendar-2025-h 8 25 "OSSEU https://events.linuxfoundation.org/open-source-summit-europe/")
                  (russian-calendar-2025-h 8 30 "UbuCon Asia https://2025.ubucon.asia/")
                  (russian-calendar-2025-h 12 30 "OSS Japan https://events.linuxfoundation.org/open-source-summit-japan-2025/")
                  (russian-calendar-2025-h 12 8 "OSS Japan https://events.linuxfoundation.org/open-source-summit-japan-2025/")
                  (russian-calendar-2025-h 12 11 "Compliance Summit https://events.linuxfoundation.org/open-compliance-summit-2025/")))))
  "Open Source conferences.")

(defvar russian-calendar-2025-ai-confs
  (mapcar #'purecopy
          '(
            (russian-calendar-2025-h 10 22 "PyTorch")
            (russian-calendar-2025-h 5 5 "IEEE CAI https://ieeecai.org/")
            (russian-calendar-2025-h 2 13 "WAIC https://www.worldaicannes.com")
            (russian-calendar-2025-h 12 10 "NeurIPS https://neurips.cc/ (will change)")))
  "AI conferences.")

(defvar russian-calendar-2025-russian-it-confs
  (mapcar #'purecopy
          '(
            (russian-calendar-2025-h 12 11 "AI Journey https://aij.ru/ (will change)")
            (russian-calendar-2025-h 11 28 "TAdviser SummIT https://tadvisersummit.ru/ (will change)")))
  "Russian IT and AI Conferences.")

(defvar russian-calendar-2025-old-slavic-fests
  (mapcar #'purecopy
          '(
            (russian-calendar-2025-h 12 21 "Карачун (смерть)")  ;; с 22 на 23 декабря 2024:21 2025:21 2026:21 2027:22 2028:21 2029:21
            (holiday-fixed 1 6 "Коляда по 19 января")
            (russian-calendar-2025-h 3 20 "Весеннее равноденствие") ;; 2024-2029:20
            (holiday-greek-orthodox-easter -55 "Масленица (неделя)") ;; в течение недели перед Великим постом
            (holiday-greek-orthodox-easter 7 "Красная горка или Лельник, поминовение усопших")
            (russian-calendar-2025-h 6 21 "Летнее солнцестояние") ;; 20 июня 2024:20 2025-2027:21 2028:20 2029:21
            (holiday-fixed 7 6 "Ночь Ивана Купалы") ;; с 6 на 7 июля
            (holiday-fixed 8 2 "Перунов день") ;; 2 августа
            (russian-calendar-2025-h 9 22 "Осеннее равноденствие") ;; 2024:22 2025:22 2026:23 2027:23 2028:22 2029:22
            (holiday-fixed 10 31 "Велесова ночь") ;; с 31 октября на 1 ноября
            ))
  "Old slavic folk holidays.")

(provide 'russian-calendar-2025)
;;; russian-calendar-2025.el ends here
