;;; russian-calendar.el --- Russian holidays and conferences. Updated 2024-11-22  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.3
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; What is included:
;; - Russian holidays
;; - International holidays: Valentine's Day, April Fools' Day, Halloween
;; - Key Orthodox Christian Holidays
;; - Old Slavic Fests
;; - Open source conferences: Emacs, FSF, GNU, FOSDEM
;; - AI and Russian IT conferences: PyTorh, NeurIPS, IEEE CAI, WAIC,
;;   AI Journey dec + TAdviser SummIT nov + CNews Forum nov
;; - common colendar configuration

;; Why? Because the dates will be updated per year at least.

;; Usage:
;; (require 'russian-calendar-2024)
;; (require 'russian-calendar)
;; (setopt calendar-holidays (append russian-calendar-holidays
;;                                   ;; - enable if you need:
;;                                   ;; russian-calendar-general-holidays
;;                                   ;; russian-calendar-orthodox-christian-holidays
;;                                   ;; russian-calendar-old-slavic-fests
;;                                   ;; russian-calendar-open-source-confs
;;                                   ;; russian-calendar-ai-confs
;;                                   ;; russian-calendar-russian-it-confs
;;                                   ))
;; ;; optional:
;; (russian-calendar-localize)
;; (russian-calendar-set-location-to-moscow)
;; (russian-calendar-show-diary-holidays-in-calendar)
;; (russian-calendar-enhance-calendar-movement)

;; Other packages:

;;; Code:

;; - May be simple:
(require 'calendar)
(require 'holidays)
(require 'cal-dst)
(require 'solar)

;; --------- 12 major Orthodox Christian Feasts ------------------

(defvar russian-calendar-orthodox-christian-holidays
  (mapcar 'purecopy
  '(
    (holiday-fixed 1 7 "Рождество Христово")
    (holiday-fixed 1 19 "Крещение Господне (Богоявление)")
    (holiday-fixed 2 15 "Сретение Господне")
    (holiday-fixed 4 7 "Благовещение Пресвятой Богородицы")
    (holiday-fixed 8 19 "Преображение Господне, Яблочный Спас")
    (holiday-fixed 8 28 "Успение Пресвятой Богородицы")
    (holiday-fixed 9 21 "Рождество Пресвятой Богородицы")
    (holiday-fixed 9 27 "Воздвижение Креста Господня")
    (holiday-fixed 12 4 "Введение во храм Пресвятой Богородицы")
    (apply 'append
           (mapcar (lambda (e)
                     (apply 'holiday-greek-orthodox-easter e))
                   (append
                    '((-48 "Чистый понедельник Великого поста")
                      ( -7 "Вход Господень в Иерусалим или Вербное воскресенье")
                      ( 40 "Вознесение Господне")
                      ( 50 "День Святой Троицы, Пятидесятница"))
                    ;; (if calendar-christian-all-holidays-flag
                    ;;     '(
                    ;;       ()
                    ;;       )
                    ;;   )
                    )))
    ;; (if calendar-christian-all-holidays-flag
    ;;     '(
    ;;       ()))
    ))
  "Orthodox christian holidays.")

(defvar russian-calendar-orthodox-christian-holidays-eng
  (mapcar 'purecopy
  '(
    (holiday-fixed 1 7 "Christmas")
    (holiday-fixed 1 19 "Epiphany")
    (holiday-fixed 2 15 "Meeting of the Lord")
    (holiday-fixed 4 7 "Annunciation")
    (holiday-fixed 8 19 "Transfiguration")
    (holiday-fixed 8 28 "Dormition of the Theotokos")
    (holiday-fixed 9 21 "Nativity of the Theotokos")
    (holiday-fixed 9 27 "Exaltation of the Cross")
    (holiday-fixed 12 4 "Presentation of the Theotokos")
    (apply 'append
           (mapcar (lambda (e)
                     (apply 'holiday-greek-orthodox-easter e))
                   (append
                    '((-48 "Clean Monday of Great Lent")
                      ( -7 "Palm Sunday")
                      ( 40 "Ascension")
                      ( 50 "Trinity Sunday"))
                    ;; (if calendar-christian-all-holidays-flag
                    ;;     '(
                    ;;       ()
                    ;;       )
                    ;;   )
                    )))
    ;; (if calendar-christian-all-holidays-flag
    ;;     '(
    ;;       ()))
    ))
  "Orthodox christian holidays.")


;; ---------------- Localizations --------------------

(defun russian-calendar-localize()
  "Translate month, days of week, etc to Russian language."
  (setq calendar-week-start-day 1
        calendar-day-name-array ["Воскресенье" "Понедельник" "Вторник" "Среда"
                                 "Четверг" "Пятница" "Суббота"]
        calendar-day-header-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
        calendar-day-abbrev-array ["Вск" "Пнд" "Втр" "Сре" "Чтв" "Птн" "Суб"]
        calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
                                   "Июнь" "Июль" "Август" "Сентябрь"
                                   "Октябрь" "Ноябрь" "Декабрь"])
  ;; - not working for unknown reason
  ;; (setq calendar-month-abbrev-array
  ;;       ["Янв" "Фев" "Мар" "Апр" "Май" "Июн" "Июл" "Авг" "Сен" "Окт" "Ноя" "Дек"])
  )

(defun russian-calendar-set-location-to-moscow ()
  "Set miscelanious option like latitude, time format."
  (setq calendar-latitude 55.7565408)
  (setq calendar-longitude 37.6149202)
  (setq calendar-location-name "Europe, Russia, Moscow")
  (setq calendar-time-zone 180) ; +3
  (setq calendar-time-display-form '(24-hours ":" minutes))
  (setq calendar-date-style 'european) ; day/month/year
  ;; (setopt calendar-weekend-days '(0 6)) ; by dafault
  )

(defun russian-calendar-show-diary-holidays-in-calendar()
 "Display holidays and diary in calendar."
  ;; (setq calendar-holiday-marker 'holiday) ; by dafault - red background
  ;; (setq diary-entry-marker 'diary) ; by default - red foreground
  (setq calendar-mark-holidays-flag t) ; show holidays in calendar
  (setq calendar-mark-diary-entries-flag t) ; show diary entries in calendar
  )

(defun russian-calendar-show-holiday (&rest r)
  "Show diary and holiday at in calendar after movement.
Optional argument R not used."
  (setq r r) ; suppress Warning: Unused lexical argument `r'
  ;; - diary
  (if (diary-list-entries (calendar-cursor-to-date t) nil)
      ;; show fancy window
      (diary-view-entries)
    ;; else - hide fancy window
    (when (get-buffer-window diary-fancy-buffer)
      (dolist (window (get-buffer-window-list diary-fancy-buffer))
        (delete-window window))))
  ;; - holidays
  (calendar-cursor-holidays))

(defun russian-calendar-enhance-calendar-movement ()
    "Display information about current diary day after movement.
With help of so called Fancy buffer of diary entires."
    (advice-add 'calendar-forward-week :after #'russian-calendar-show-holiday)
    (advice-add 'calendar-backward-week :after #'russian-calendar-show-holiday)
    (advice-add 'calendar-forward-day :after #'russian-calendar-show-holiday)
    (advice-add 'calendar-backward-day :after #'russian-calendar-show-holiday))

(provide 'russian-calendar)
;;; russian-calendar.el ends here
