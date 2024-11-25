;;; russian-calendar.el --- Russian holidays and conferences. Updated 2024-11-22  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.2
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
;; What Include:
;; - Russian holidays
;; - International holidays: Valentine's Day, April Fools' Day, Halloween
;; - Open source conferences: Emacs, FSF, GNU, FOSDEM
;; - AI and Russian IT conferences: PyTorh, NeurIPS, IEEE CAI, WAIC,
;;   AI Journey dec + TAdviser SummIT nov + CNews Forum nov

;; Why? Because the dates will be updated per year at least.

;; Usage:
;; (require 'russian-calendar)
;; (setopt calendar-holidays (append russian-calendar-holidays
;;                                   russian-calendar-general-holidays
;;                                   ;; - enable if you need:
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
(require 'cal-dst)
(require 'solar)
(require 'russian-calendar-2024)

;; - Check that we are are at right year.
(defun get-current-year ()
  "Return the current year."
  (nth 5 (decode-time (current-time))))

(if (not (= (get-current-year) 2024))
    (message "Please update package \"russian-calendar\". \n \
Or set (require 'russian-calendar-2025) \n \
Happy new 2025 year!"))


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
    "Display information about current day after movement."
    (advice-add 'calendar-forward-week :after #'russian-calendar-show-holiday)
    (advice-add 'calendar-backward-week :after #'russian-calendar-show-holiday)
    (advice-add 'calendar-forward-day :after #'russian-calendar-show-holiday)
    (advice-add 'calendar-backward-day :after #'russian-calendar-show-holiday))

(provide 'russian-calendar)
;;; russian-calendar.el ends here
