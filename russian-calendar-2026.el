;;; russian-calendar-2026.el --- Holidays and conferences  -*- lexical-binding: t -*-

;; Copyright (c) 2026 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.1.0

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

(defun russian-calendar-2026-h (month day string)
  "Holiday fixed-limited for 2024 year only.
Arguments MONTH, DAY, STRING is standard arguments for
`holiday-fixed' function.  Return formatted date or nil."
  (if (= (bound-and-true-p displayed-year) 2026)
      (holiday-fixed month day string)))

;; (defalias 'holidayf-2026 'russian-calendar-holiday-fixed-2026)

(defvar russian-calendar-2026-holidays
  (mapcar #'purecopy
          '(
          (holiday-fixed 1 1 "Вых. Новогодние каникулы")
          (holiday-fixed 1 2 "Вых. Новогодние каникулы")
          (holiday-fixed 1 3 "Вых. Новогодние каникулы")
          (holiday-fixed 1 4 "Вых. Новогодние каникулы")
          (holiday-fixed 1 5 "Вых. Новогодние каникулы")
          (holiday-fixed 1 6 "Вых. Новогодние каникулы")
          (holiday-fixed 1 7 "Вых. Рождество Христово")
          (holiday-fixed 1 8 "Вых. Новогодние каникулы")
          (holiday-fixed 1 9 "Вых. Новогодние каникулы")
          (holiday-fixed 2 23 "Вых. День защитника Отечества")
          (russian-calendar-2026-h 3 7 "* Сокращённый день перед Международный женский день")
          (holiday-fixed 3 8 "Вых. Международный женский день")
          (russian-calendar-2026-h 3 9 "Вых. после Международный женский день")
          (russian-calendar-2026-h 4 30 "* Сокращённый день перед Праздник Весны и Труда")
          (holiday-fixed 5 1 "Вых. Праздник Весны и Труда")
          (russian-calendar-2026-h 5 8 "* Сокращённый день перед День Победы")
          (holiday-fixed 5 9 "Вых. День Победы")
          (russian-calendar-2026-h 5 11 "Вых. после День Победы")
          (russian-calendar-2026-h 6 11 "* Сокращённый день перед День России")
          (holiday-fixed 6 12 "Вых. День России")
          (russian-calendar-2026-h 11 1 "! Рабочий день за 3")
          (russian-calendar-2026-h 11 3 "* Сокращённый день перед День народного единства")
          (holiday-fixed 11 4 "Вых. День народного единства")
          (holiday-fixed 12 31 "Вых. перед Новый год")))
  "Russian calendar holidays.")

(defvar russian-calendar-2026-open-source-confs
  (mapcar #'purecopy
          '(
            (russian-calendar-2026-h 1 31  "FOSDEM https://fosdem.org")
            (russian-calendar-2026-h 12 6 "EmacsConf https://emacsconf.org/ (will change)")
            ;; (when russian-calendar-all-open-source-confs-flag
            ;;     (append
            ;;       (russian-calendar-2026-h 1 20 "Australia. Open technologies. https://2026.everythingopen.au/")
            ;;       (russian-calendar-2026-h 3 6 "Australia. SCALE 22x https://www.socallinuxexpo.org/scale/22x")
            ;;       (russian-calendar-2026-h 3 9 "Southern California Linux Expo. SCALE 22x https://www.socallinuxexpo.org/scale/22x")
            ;;       (russian-calendar-2026-h 3 10 "FOSS Backstage Netdev 0x19 https://25.foss-backstage.de/")
            ;;       (russian-calendar-2026-h 3 18 "Nordic PGDay pgDay Paris https://2026.nordicpgday.org/, LF Member Summit https://events.linuxfoundation.org/lf-member-summit/")
            ;;       (russian-calendar-2026-h 3 20 "pgDay Paris https://2026.pgday.paris/")
            ;;       (russian-calendar-2026-h 3 20 "LSFMM https://events.linuxfoundation.org/lsfmmbpf/")
            ;;       (russian-calendar-2026-h 4 9 "sambaXP 2026 https://sambaxp.org/")
            ;;       (russian-calendar-2026-h 4 14 "foss-north https://foss-north.se/2026/")
            ;;       (russian-calendar-2026-h 4 26 "LIT Augsburg https://www.luga.de/LIT-2026/")
            ;;       (russian-calendar-2026-h 4 29 "stackconf https://stackconf.eu/")
            ;;       (russian-calendar-2026-h 5 8 "PGConf.de https://2026.pgconf.de/")
            ;;       (russian-calendar-2026-h 5 13 "PGConf.dev https://2026.pgconf.dev/")
            ;;       (russian-calendar-2026-h 5 16 "PyCon US (Pittsburgh, Pennsylvania) https://us.pycon.org/2026/")
            ;;       (russian-calendar-2026-h 5 22 "NLUUG Spring Conference 2026 https://nluug.nl/")
            ;;       (russian-calendar-2026-h 6 23 "OSSNA https://events.linuxfoundation.org/open-source-summit-north-america/")
            ;;       (russian-calendar-2026-h 6 26 "LSSNA https://events.linuxfoundation.org/linux-security-summit-north-america/, LAC https://jimlac25.inria.fr/lac/")
            ;;       (russian-calendar-2026-h 6 26 "LAC https://jimlac25.inria.fr/lac/")
            ;;       (russian-calendar-2026-h 7 31 "FOSSY https://2026.fossy.us/")
            ;;       (russian-calendar-2026-h 8 5 "OSS India https://events.linuxfoundation.org/open-source-summit-india/")
            ;;       (russian-calendar-2026-h 8 25 "OSSEU https://events.linuxfoundation.org/open-source-summit-europe/")
            ;;       (russian-calendar-2026-h 8 30 "UbuCon Asia https://2026.ubucon.asia/")
            ;;       (russian-calendar-2026-h 12 30 "OSS Japan https://events.linuxfoundation.org/open-source-summit-japan-2026/")
            ;;       (russian-calendar-2026-h 12 8 "OSS Japan https://events.linuxfoundation.org/open-source-summit-japan-2026/")
            ;;       (russian-calendar-2026-h 12 11 "Compliance Summit https://events.linuxfoundation.org/open-compliance-summit-2026/")))
            ))
  "Open Source conferences.")

(defvar russian-calendar-2026-ai-confs
  (mapcar #'purecopy
          '(
            (russian-calendar-2026-h 2 12 "WAIC https://www.worldaicannes.com")
            (russian-calendar-2026-h 7 14 "ACML - Tutorials https://icml.cc/ (will change)")
            (russian-calendar-2026-h 10 22 "PyTorch (will change)")
            (russian-calendar-2026-h 12 10 "NeurIPS https://neurips.cc/ (will change)")))
  "AI conferences.")

(defvar russian-calendar-2026-russian-it-confs
  (mapcar #'purecopy
          '(
            (russian-calendar-2026-h 12 11 "AI Journey https://aij.ru/ (will change)")
            (russian-calendar-2026-h 11 28 "TAdviser SummIT https://tadvisersummit.ru/ (will change)")
            (russian-calendar-2026-h 11 11 "CNews Forum 2026: Information technology tomorrow https://cnewsforum.ru/forum/ (will change)")))
  "Russian IT and AI Conferences.")

(provide 'russian-calendar-2026)
;;; russian-calendar-2026.el ends here
