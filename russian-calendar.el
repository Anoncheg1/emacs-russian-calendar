;;; russian-calendar.el --- Russian holidays and conferences. Updated 2025-09-30  -*- lexical-binding: t -*-

;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.4"))

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
;; 2025  (updated 24/09/25)
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

;; Features:
;; - Support arbitrary number of years at once
;; - fix for behavior of `list-holidays' function included
;; - this package is example of how to set holidays per year
;
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
;; (russian-calendar-check-year-not-obsolate)

;; Other packages:
;; - Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
;; - Search with Chinese			https://github.com/Anoncheg1/pinyin-isearch
;; - Ediff fix					https://github.com/Anoncheg1/ediffnw
;; - Dired history				https://github.com/Anoncheg1/dired-hist
;; - Selected window contrast			https://github.com/Anoncheg1/selected-window-contrast
;; - Copy link to clipboard			https://github.com/Anoncheg1/org-links
;; - Solution for "callback hell"		https://github.com/Anoncheg1/emacs-async1

;; *DONATE MONEY*:
;; You can sponsor author directly with crypto currencies:
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D


;;; Code:

(require 'calendar)
(require 'holidays)
(require 'cal-dst)
(require 'solar)
(require 'russian-calendar-2025)
(require 'russian-calendar-2026)

(defcustom russian-calendar-all-open-source-confs-flag nil
  "Non-nil means enable all open source conferences.
Otherwise only FOSDEM and EmacsConf enabled"
  :group 'russian-calendar
  :type 'boolean)

;; - required by `russian-calendar-eqsols' and
;; - `russian-calendar-calendar-holiday-list-slide'
(defvar displayed-month)
(defvar displayed-year)

;; --- --- --- Concat years --- --- ---

(defun russian-calendar-concat (&rest args)
  "Concat arbitrary ARGS holidays definitions.  Years mostly."
  (seq-uniq
   (seq-copy (apply #'append args))))

(defvar russian-calendar-holidays
  (russian-calendar-concat russian-calendar-2025-holidays
                           russian-calendar-2026-holidays)
  "Production calendar.")
(defvar russian-calendar-open-source-confs
    (russian-calendar-concat russian-calendar-2025-open-source-confs
                             russian-calendar-2026-open-source-confs)
    "Conferences in field of open technologies.")
(defvar russian-calendar-ai-confs
    (russian-calendar-concat russian-calendar-2025-ai-confs
                             russian-calendar-2026-ai-confs)
    "Conferences in field of AI technologies.")
(defvar russian-calendar-russian-it-confs
    (russian-calendar-concat russian-calendar-2025-russian-it-confs
                             russian-calendar-2026-russian-it-confs)
    "Russia IT conferences.")
;; --- --- --- General International Holidays

(defvar russian-calendar-general-holidays
  (mapcar #'purecopy
          '(
            (holiday-fixed 2 14 "Valentine's Day")
            (holiday-fixed 4 1 "April Fools' Day")
            (holiday-fixed 10 31 "Halloween")))
  "International holidays.")

;; --- --- --- Key Orthodox Christian Feasts

(defvar russian-calendar-orthodox-christian-holidays
  (mapcar #'purecopy
  '(
    (holiday-fixed 1  7 "Рождество Христово")
    (holiday-fixed 1 19 "Крещение Господне (Богоявление)")
    (holiday-fixed 2 15 "Сретение Господне")
    (holiday-fixed 4  7 "Благовещение Пресвятой Богородицы")
    (holiday-fixed 8 19 "Преображение Господне, Яблочный Спас")
    (holiday-fixed 8 28 "Успение Пресвятой Богородицы")
    (holiday-fixed 9 21 "Рождество Пресвятой Богородицы")
    (holiday-fixed 9 27 "Воздвижение Креста Господня")
    (holiday-fixed 12 4 "Введение во храм Пресвятой Богородицы")
    (apply #'append
           (mapcar (lambda (e)
                     (apply #'holiday-greek-orthodox-easter e))
                   (append
                    '((-48 "Чистый понедельник Великого поста")
                      ( -7 "Вход Господень в Иерусалим, Вербное воскресенье, Страстная седмица")
                      (  0 "Пасха, Светлое Христово Воскресение")
                      (  9 "Радоница")
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
  (mapcar #'purecopy
  '(
    (holiday-fixed 1  7 "Christmas")
    (holiday-fixed 1 19 "Epiphany")
    (holiday-fixed 2 15 "Meeting of the Lord")
    (holiday-fixed 4  7 "Annunciation")
    (holiday-fixed 8 19 "Transfiguration")
    (holiday-fixed 8 28 "Dormition of the Theotokos")
    (holiday-fixed 9 21 "Nativity of the Theotokos")
    (holiday-fixed 9 27 "Exaltation of the Cross")
    (holiday-fixed 12 4 "Presentation of the Theotokos")
    (apply #'append
           (mapcar (lambda (e)
                     (apply #'holiday-greek-orthodox-easter e))
                   (append
                    '((-48 "Clean Monday of Great Lent")
                      ( -7 "Palm Sunday, Holy Week")
                      (  0 "Easter Sunday, Resurrection of Jesus")
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


;; --- --- --- Old Slavic Folk Holidays
(defun russian-calendar-eqsols (qr string)
  "Date of equinox/solstice QR for displayed-year of calendar.
The return value has the form ((MONTH DAY YEAR) STRING)."
  (let* ((date (solar-equinoxes/solstices qr displayed-year))
         (month (nth 0 date))
         (day (truncate (nth 1 date))))
    (holiday-fixed month day string)))

(defvar russian-calendar-old-slavic-fests
  (mapcar #'purecopy
          '(
            (russian-calendar-eqsols 0 "Весеннее равноденствие") ;; 2024-2029:20
            (russian-calendar-eqsols 1 "Летнее солнцестояние") ;; 20 июня 2024:20 2025-2027:21 2028:20 2029:21
            (russian-calendar-eqsols 2 "Осеннее равноденствие") ;; 2024:22 2025:22 2026:23 2027:23 2028:22 2029:22
            (russian-calendar-eqsols 3 "Карачун (смерть)")  ;; с 22 на 23 декабря 2024:21 2025:21 2026:21 2027:22 2028:21 2029:21
            (holiday-fixed 1 6 "Коляда по 19 января")
            (holiday-greek-orthodox-easter -55 "Масленица (неделя)") ;; в течение недели перед Великим постом
            (holiday-greek-orthodox-easter 7 "Красная горка или Лельник, поминовение усопших")
            (holiday-fixed 7 6 "Ночь Ивана Купалы") ;; с 6 на 7 июля
            (holiday-fixed 8 2 "Перунов день") ;; 2 августа
            (holiday-fixed 10 31 "Велесова ночь") ;; с 31 октября на 1 ноября
            ))
  "Old slavic folk holidays.")
;; --- --- --- Localizations And Configurations

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

;; --- --- --- Available-holiday list --- --- ---

(defun russian-calendar-available-holidays ()
  "Return a list of all holiday lists.
This is used by `list-holidays'.  For 29.3 require fix."
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
    (cons "Ask" nil))))

(defun russian-calendar-available-holidays-eng ()
  "Return a list of all holiday lists.
This is used by `list-holidays'."
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

;; --- --- --- Enhance-calendar-movement --- --- ---

(defun russian-calendar-show-holiday-advice (&rest r)
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
    (advice-add 'calendar-forward-week :after #'russian-calendar-show-holiday-advice)
    (advice-add 'calendar-backward-week :after #'russian-calendar-show-holiday-advice)
    (advice-add 'calendar-forward-day :after #'russian-calendar-show-holiday-advice)
    (advice-add 'calendar-backward-day :after #'russian-calendar-show-holiday-advice))

;; --- --- --- Fix list-holidays --- --- ---

(defun russian-calendar-calendar-holiday-list (&optional holidays)
  "Form the list of HOLIDAYS that occur on dates in the calendar window.
The holidays are those in the list `calendar-holidays'."
  (let ((holidays (or holidays calendar-holidays))
         res
         h)
    (sort
     (dolist (p holidays res)
       (if (setq h (if calendar-debug-sexp
                       (let ((debug-on-error t))
                         (eval p t))
                     (condition-case err
                         (eval p t)
                       (error
                        (display-warning
                         'holidays
                         (format "Bad holiday list item: %s\nError: %s\n"
                                 p err)
                         :error)
                        nil))))
           (setq res (append h res))))
     'calendar-date-compare)))

(defun russian-calendar-calendar-display-holidays (holiday-list title y1 y2)
  "Create or switch to `holiday-buffer' and write list of holidays.
Argument HOLIDAY-LIST is formatted holidays, TITLE is a modeline
for buffer, Y1 nad Y2 is a begining and end of year.  Return nothing.  To
test: (calendar-display-holidays (calendar-holiday-list-slide
calendar-holidays 2024 2025) \"test\" 2024 2025)."
  (save-current-buffer
      (calendar-in-read-only-buffer holiday-buffer
        (with-selected-window (get-buffer-window holiday-buffer)
          (calendar-set-mode-line
           (if (= y1 y2)
               (format "%s for %s" title y1)
             (format "%s for %s-%s" title y1 y2))))
        (insert
         (mapconcat
          (lambda (x) (concat (calendar-date-string (car x))
                              ": " (cadr x)))
          holiday-list "\n")))))

(defun russian-calendar-calendar-holiday-list-slide (holidays year-begin year-end)
  "Wrap for `calendar-holiday-list' that don't filter dates.
Function `calendar-holiday-list' calls `holiday-fixed' and other
functions that use displayed-month and displayed-year and assume
that calendar are opened to filter only visible dates.  This
function is kind of hack to get all dates without filter.  This
works by sliding displayed-month and displayed-year.
Argument HOLIDAYS is `calendar-holidays', YEAR-BEGIN and YEAR-END is a
range to filter dates.
Test: (mapc
print (calendar-holiday-list-slide calendar-holidays 2024 2025))"
(let ((original-month (and (boundp 'displayed-month) displayed-month))
      (original-year (and (boundp 'displayed-year) displayed-year))
      (s (calendar-absolute-from-gregorian (list 2 1 year-begin)))
      (e (calendar-absolute-from-gregorian (list 11 1 year-end)))
      (calendar-holidays holidays)  ; rebind for `calendar-holiday-list'
      holiday-list)
  (setq displayed-month 2) ; rebing for `calendar-holiday-list'
  (setq displayed-year year-begin) ; rebind for `calendar-holiday-list'
  (while (<= s e) ; loop every 3 month
      (setq holiday-list (append holiday-list (russian-calendar-calendar-holiday-list)))
      (calendar-increment-month displayed-month displayed-year 3)
      (setq s (calendar-absolute-from-gregorian
               (list displayed-month 1 displayed-year))))
  (if original-month
      (setq displayed-month original-month)
    ;; else
    (makunbound 'displayed-month))
  (if original-year
      (setq displayed-month original-year)
    ;; else
    (makunbound 'displayed-year))
  holiday-list ; return
  ))


(defun russian-calendar-list-holidays (start-year &optional end-year hdays label)
  "Display holidays in a new window.
May be called within calendar and outside.  START-YEAR and
END-YEAR required for proper working of calendar functions.
END-YEAR defaults to START-YEAR.  The optional list of holidays
HDAYS defaults to `calendar-holidays'.  If you want to control
what holidays are displayed, use a different list.  For example,

  (list-holidays 2006 2006
    (append holiday-general-holidays holiday-local-holidays))

will display holidays for the year 2006 defined in the two
mentioned lists, and nothing else.

When called interactively, this command offers a choice of
holidays, based on the variables `holiday-solar-holidays' etc.  See the
documentation of `calendar-holidays' for a list of the variables
that control the choices, as well as a description of the format
of a holiday list.

The optional LABEL is used to label the buffer created.

The list of holiday lists is computed by the
`holiday-available-holiday-lists' and you can alter the results
by redefining that function, or use `add-function' to add
values."
  (interactive
   (let* ((in-calendar-p (and (boundp 'displayed-month)
                              (boundp 'displayed-year)))
          (start-year (if in-calendar-p
                          (bound-and-true-p displayed-year) ; don't ask in calendar
                        ;; else - outside of calendar
                        (calendar-read-sexp
                         "Starting year of holidays (>0)"
                         (lambda (x) (> x 0))
                         (calendar-extract-year (calendar-current-date)))))
          (end-year (if in-calendar-p
                        (bound-and-true-p displayed-year) ; don't ask in calendar
                      ;; else - outside of calendar
                      (calendar-read-sexp
                       "Ending year (inclusive) of holidays (>=%s)"
                       (lambda (x) (>= x start-year))
                       start-year
                       start-year)))
          ; - common for in calendar and outside of calendar
          (lists (holiday-available-holiday-lists))
          (choice ; (capitalize - FIX: removed capitalize
           (completing-read "List (TAB for choices): " lists nil t))
          (days-selected (if (string-equal choice "Ask")
                     (symbol-value (read-variable "Enter list name: "))
                   (cdr (assoc choice lists))))
          (name (if (string-equal choice "Equinoxes/Solstices")
                    choice
                  (if (member choice '("Ask" ""))
                      "Holidays"
                    (format "%s Holidays" choice)))))
     (list start-year end-year days-selected name)))
  ;; end-year, hdays, label - may be nil
  (unless end-year
    (setq end-year start-year))
  (unless hdays
    (setq hdays calendar-holidays))
  (unless label
    (setq label "Holidays"))

  (if (and (boundp 'displayed-month)
           (boundp 'displayed-year))
      ;; - in calendar case
      (russian-calendar-calendar-display-holidays (russian-calendar-calendar-holiday-list hdays)
                                                  label
                                                  displayed-year
                                                  displayed-year)
    ;; don't select buffer with holidays
    ;; - else:
    ;; case for outside of calendar
    (russian-calendar-calendar-display-holidays (russian-calendar-calendar-holiday-list-slide hdays start-year end-year)
                                                label
                                                start-year
                                                end-year)
    ;; select buffer with holidays
    (select-window (get-buffer-window holiday-buffer))))

(defun russian-calendar-fix-list-holidays ()
  "Fix `list-holidays' bugs: capitalize, displayed-month=2."
  (advice-add 'list-holidays :override #'russian-calendar-list-holidays)
  (advice-add 'holiday-available-holiday-lists :override #'russian-calendar-available-holidays))

;; --- --- --- Check year --- --- ---

(defun russian-calendar-check-year-not-obsolate ()
  "Check that `russian-calendar' package support this year."
  (let ((cyear (number-to-string
                ;; get current year
                (nth 5 (decode-time (current-time))))))
    ;; Do we obsolate?
    (if (not (boundp (intern (concat "russian-calendar-" cyear "-holidays"))))
        (error "Package russian-calendar is obsolate, please update"))))
;; --- --- --- footer --- --- ---
(provide 'russian-calendar)
;;; russian-calendar.el ends here
