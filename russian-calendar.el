;;; russian-calendar.el --- Russian holidays and conferences. Updated 2024-11-22  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.4
;; Package-Requires: ((emacs "29.1"))

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
;;
;; 2024, 2025  (updated 22/11/24)
;;
;; What:
;; - Russian holidays
;; - International holidays: Valentine's Day, April Fools' Day, Halloween
;; - Key Orthodox Christian Holidays
;; - Old Slavic Fests
;; - Open source conferences: Emacs, FSF, GNU, FOSDEM
;; - AI and Russian IT conferences: PyTorh, NeurIPS, IEEE CAI, WAIC,
;;   AI Journey dec + TAdviser SummIT nov + CNews Forum nov
;; - common colendar configuration

;; Why? Because the dates will be updated per year at least.

;; Not included: regional holidays.

;; Usage:
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
;; (russian-calendar-fix-list-holidays)

;; Features
;; - select current year at loading time
;; - if year > 2025, then signat that update required

;; Other packages:

;;; Code:

;; - May be simple:
(require 'calendar)
(require 'holidays)
(require 'cal-dst)
(require 'solar)
(require 'russian-calendar-2024)
(require 'russian-calendar-2025)


(let ((cyear (number-to-string
              ; get current year
              (nth 5 (decode-time (current-time))))))
  ;; Do we obsolate?
  (if (not (boundp (intern (concat "russian-calendar-" cyear "-holidays"))))
      (error "Package russian-calendar is obsolate, please update"))

  (defvaralias 'russian-calendar-holidays
    (intern (concat "russian-calendar-" cyear "-holidays")))
  (defvaralias 'russian-calendar-general-holidays
    (intern (concat "russian-calendar-" cyear "-general-holidays")))
  (defvaralias 'russian-calendar-open-source-confs
    (intern (concat "russian-calendar-" cyear "-open-source-confs")))
  (defvaralias 'russian-calendar-ai-confs
    (intern (concat "russian-calendar-" cyear "-ai-confs")))
   (defvaralias 'russian-calendar-russian-it-confs
    (intern (concat "russian-calendar-" cyear "-russian-it-confs")))
   (defvaralias 'russian-calendar-old-slavic-fests
    (intern (concat "russian-calendar-" cyear "-old-slavic-fests"))))


;; --------- Key Orthodox Christian Feasts ------------------

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
                      ( -7 "Вход Господень в Иерусалим, Вербное воскресенье, Страстная седмица")
                      (0 "Пасха, Светлое Христово Воскресение")
                      ( 9 "Радоница")
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
                      ( -7 "Palm Sunday, Holy Week")
                      (0 "Easter Sunday, Resurrection of Jesus")
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

(defun russian-calendar-holiday-available-holiday-lists-eng ()
  "Return a list of all holiday lists.
This is used by `list-holidays'."
  ;; (with-no-warnings
  (delq
   nil
   (list
    (cons "All" calendar-holidays)
    (if (bound-and-true-p russian-calendar-holidays)
        (cons "Production Calendar" russian-calendar-holidays)) ; "Производственный календарь"
    (if (bound-and-true-p russian-calendar-general-holidays)
        (cons "General International" russian-calendar-general-holidays)) ; "Междунородные праздники"
    (if (bound-and-true-p russian-calendar-orthodox-christian-holidays)
        (cons "Orthodox Christian" russian-calendar-orthodox-christian-holidays)) ; "Православные праздники"
    (if (bound-and-true-p russian-calendar-old-slavic-fests)
        (cons "Old Slavic Fests" russian-calendar-old-slavic-fests)) ; "Старославянские праздники"
    (if (bound-and-true-p russian-calendar-open-source-confs)
        (cons "F.O.S.S Confs" russian-calendar-open-source-confs)) ; "FOSS Конференции"
    (if (bound-and-true-p russian-calendar-ai-confs)
        (cons "A.I Confs" russian-calendar-ai-confs)) ; "AI конференции"
    (if (bound-and-true-p russian-calendar-russian-it-confs)
        (cons "Russian I.T Confs" russian-calendar-russian-it-confs)) ; "Русские IT конференции"
    (cons "Ask" nil))))

(defun russian-calendar-holiday-available-holiday-lists ()
  "Return a list of all holiday lists.
This is used by `list-holidays'.  For 29.3 require fix."
  (with-no-warnings
  (delq
   nil
   (list
    (cons "All" calendar-holidays)
    (if (bound-and-true-p russian-calendar-holidays)
        (cons "Производственный календарь" russian-calendar-holidays)) ; "Production Calendar"
    (if (bound-and-true-p russian-calendar-general-holidays)
        (cons "Междунородные праздники" russian-calendar-general-holidays)) ; "General International"
    (if (bound-and-true-p russian-calendar-orthodox-christian-holidays)
        (cons "Православные праздники" russian-calendar-orthodox-christian-holidays)) ; "Orthodox Christian"
    (if (bound-and-true-p russian-calendar-old-slavic-fests)
        (cons "Старославянские праздники" russian-calendar-old-slavic-fests)) ; "Old Slavic Fests"
    (if (bound-and-true-p russian-calendar-open-source-confs)
        (cons "FOSS Конференции" russian-calendar-open-source-confs)) ; "FOSS Confs"
    (if (bound-and-true-p russian-calendar-ai-confs)
        (cons "AI конференции" russian-calendar-ai-confs)) ; "AI Confs"
    (if (bound-and-true-p russian-calendar-russian-it-confs)
        (cons "Русские IT конференции" russian-calendar-russian-it-confs)) ; "Russian IT Confs"
    (cons "Ask" nil)))))

(defun russian-calendar-list-holidays (y1 &optional y2 l label)
"Fixed version.
With fix of removed capitalize and not displayed-month=2.
Argument Y1 start-year.
Optional argument Y2 end-year.
Optional argument L list of holidays for selected type.
Optional argument LABEL name of holidays type."
  (interactive
   (let* ((start-year (calendar-read-sexp
                       "Starting year of holidays (>0)"
                       (lambda (x) (> x 0))
                       (calendar-extract-year (calendar-current-date))))
          (end-year (calendar-read-sexp
                     "Ending year (inclusive) of holidays (>=%s)"
                     (lambda (x) (>= x start-year))
                     start-year
                     start-year))
          (completion-ignore-case t)
          (lists (holiday-available-holiday-lists))
          (choice ; (capitalize - FIX: removed capitalize
                   (completing-read "List (TAB for choices): " lists nil t))
          (which (if (string-equal choice "Ask")
                     (symbol-value (read-variable "Enter list name: "))
                   (cdr (assoc choice lists))))
          (name (if (string-equal choice "Equinoxes/Solstices")
                    choice
                  (if (member choice '("Ask" ""))
                      "Holidays"
                    (format "%s Holidays" choice)))))
     (list start-year end-year which name)))
  (unless y2 (setq y2 y1))
  (message "Computing holidays...")
  (let ((calendar-holidays (or l calendar-holidays))
        (title (or label "Holidays"))
        (s (calendar-absolute-from-gregorian (list 2 1 y1)))
        (e (calendar-absolute-from-gregorian (list 11 1 y2)))
        (displayed-month (or (bound-and-true-p displayed-month) 2)) ; FIX displayed-month=2
        (displayed-year y1)
        holiday-list)
    (while (<= s e)
      (setq holiday-list (append holiday-list (calendar-holiday-list)))
      (calendar-increment-month displayed-month displayed-year 3)
      (setq s (calendar-absolute-from-gregorian
               (list displayed-month 1 displayed-year))))
    (save-current-buffer
      (calendar-in-read-only-buffer holiday-buffer
        (calendar-set-mode-line
         (if (= y1 y2)
             (format "%s for %s" title y1)
           (format "%s for %s-%s" title y1 y2)))
        (insert
         (mapconcat
          (lambda (x) (concat (calendar-date-string (car x))
                              ": " (cadr x)))
          holiday-list "\n")))
      (message "Computing holidays...done"))))


(defun russian-calendar-fix-list-holidays ()
  "Fix `list-holidays' bugs: capitalize, displayed-month=2."
  (advice-add 'list-holidays :override #'russian-calendar-list-holidays)
  (advice-add 'holiday-available-holiday-lists :override #'russian-calendar-holiday-available-holiday-lists))

(provide 'russian-calendar)
;;; russian-calendar.el ends here
