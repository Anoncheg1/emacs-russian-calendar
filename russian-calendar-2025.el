;;; russian-calendar-2025.el --- Holidays and conferences  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.6

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
            (holidayf-2025 2 1  "FOSDEM https://fosdem.org")
            (holidayf-2025 12 7 "EmacsConf https://emacsconf.org/ (will change)")
            (if russian-calendar-all-open-source-confs-flag
                (append
                  (holidayf-2025 1 20 "Australia. Open technologies. https://2025.everythingopen.au/")
                  (holidayf-2025 3 6 "Australia. SCALE 22x https://www.socallinuxexpo.org/scale/22x")
                  (holidayf-2025 3 9 "Southern California Linux Expo. SCALE 22x https://www.socallinuxexpo.org/scale/22x")
                  (holidayf-2025 3 10 "FOSS Backstage Netdev 0x19 https://25.foss-backstage.de/")
                  (holidayf-2025 3 18 "Nordic PGDay pgDay Paris https://2025.nordicpgday.org/, LF Member Summit https://events.linuxfoundation.org/lf-member-summit/")
                  (holidayf-2025 3 20 "pgDay Paris https://2025.pgday.paris/")
                  (holidayf-2025 3 20 "LSFMM https://events.linuxfoundation.org/lsfmmbpf/")
                  (holidayf-2025 4 9 "sambaXP 2025 https://sambaxp.org/")
                  (holidayf-2025 4 14 "foss-north https://foss-north.se/2025/")
                  (holidayf-2025 4 26 "LIT Augsburg https://www.luga.de/LIT-2025/")
                  (holidayf-2025 4 29 "stackconf https://stackconf.eu/")
                  (holidayf-2025 5 8 "PGConf.de https://2025.pgconf.de/")
                  (holidayf-2025 5 13 "PGConf.dev https://2025.pgconf.dev/")
                  (holidayf-2025 5 16 "PyCon US (Pittsburgh, Pennsylvania) https://us.pycon.org/2025/")
                  (holidayf-2025 5 22 "NLUUG Spring Conference 2025 https://nluug.nl/")
                  (holidayf-2025 6 23 "OSSNA https://events.linuxfoundation.org/open-source-summit-north-america/")
                  (holidayf-2025 6 26 "LSSNA https://events.linuxfoundation.org/linux-security-summit-north-america/, LAC https://jimlac25.inria.fr/lac/")
                  (holidayf-2025 6 26 "LAC https://jimlac25.inria.fr/lac/")
                  (holidayf-2025 7 31 "FOSSY https://2025.fossy.us/")
                  (holidayf-2025 8 5 "OSS India https://events.linuxfoundation.org/open-source-summit-india/")
                  (holidayf-2025 8 25 "OSSEU https://events.linuxfoundation.org/open-source-summit-europe/")
                  (holidayf-2025 8 30 "UbuCon Asia https://2025.ubucon.asia/")
                  (holidayf-2025 12 30 "OSS Japan https://events.linuxfoundation.org/open-source-summit-japan-2025/")
                  (holidayf-2025 12 8 "OSS Japan https://events.linuxfoundation.org/open-source-summit-japan-2025/")
                  (holidayf-2025 12 11 "Compliance Summit https://events.linuxfoundation.org/open-compliance-summit-2025/")
                  ))))
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
