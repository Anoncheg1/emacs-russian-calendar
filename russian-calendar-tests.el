;;; russian-calendar-tests.el --- Russian holidays and conferences. Updated 2024-11-22  -*- lexical-binding: t -*-
;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: calendar, holidays
;; URL: https://github.com/Anoncheg1/emacs-russian-calendar
;; Version: 0.0.6
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
;; to run: emacs -batch -l ert -l pinyin-isearch.el -l pinyin-isearch-pinyin-tests.el -f ert-run-tests-batch-and-exit 2> out.log
;; eval-buffer
;; M-x ert RET t RET

;; Raw state

;;; Code:

(require 'ert)
(require 'russian-calendar)

;; (defvar russian-calendar-2024-holidays
;;   (mapcar 'purecopy
;;           '(
;;             ;; was:
;;             ;; (((12 28 2024) "Рабочий день"))
;;             ;; become:
;;             ;; '((12 28 2024) "Рабочий день")
;;             ;; '(list '(1 0 2024) "Р")
;;             '(((12 28 2024) "Рабочий день2"))
;;             '(((2 30 2024) "Рабочий день2"))
;;             '(((12 30 2024) "Вых. Выходной"))
;;             (holiday-fixed 1 31 "Вых. перед Новый год")
;;             (holiday-fixed 1 1 "Вых. перед Н")
;;             )
;;           )
;;   "Russian calendar holidays.")

(russian-calendar-calendar-holiday-list-slide
                                                 russian-calendar-2024-holidays
                                                 ;; holiday-christian-holidays
                                                 2024
                                                 2024
                                                 )

(ert-deftest russian-calendar--test1 ()
  (with-temp-buffer
    (setq displayed-month 1
          displayed-year 2024)
    ;; ;; ------- 1
    ;; (let ((holidays '((holiday-fixed 1 6 "Epiphany")
    ;;                   (holiday-fixed 2 9 "Радоница")
    ;;                   (apply 'append
    ;;                          (mapcar (lambda (e)
    ;;                                    (apply 'holiday-greek-orthodox-easter e))
    ;;                                  (append
    ;;                                   '((-48 "Чистый понедельник Великого поста")
    ;;                                     ( 50 "День Святой Троицы, Пятидесятница"))
    ;;                 )))
    ;;                   '(((1 6 2024) "Epiphany2"))
    ;;                   ;; ( 1 6 "Epiphany")
    ;;                   )))
    ;;   (dolist (p holidays)
    ;;     (print (eval p t))))
    ;; ;; -------- 2
    ;; (should (equal (eval '(list '(1 6 2024) "Epiphany") t)
    ;;               '((1 6 2024) "Epiphany")))
    ;; ;; ------- 3
    ;; (russian-calendar-calendar-display-holidays (russian-calendar-calendar-holiday-list-slide
    ;;                                              russian-calendar-2024-holidays
    ;;                                              ;; holiday-christian-holidays
    ;;                                              2024
    ;;                                              2024
    ;;                                              ) "test" 2024 2022)
    ;; ;; ------ 4
    ;; (let (res
    ;;       h)
    ;;   (dolist (p russian-calendar-2024-holidays res)
    ;;     (setq h (eval p t))
    ;;     (setq res (append h res))
    ;;     ;; (print res)
    ;;     )
    ;;   (print res)
    ;;   (setq res (sort res

    ;;         'calendar-date-compare
    ;;         ;; (lambda (date1 date2)
    ;;         ;;   (print (list date1 date2 (< (calendar-absolute-from-gregorian (car date1))
    ;;         ;;                               (calendar-absolute-from-gregorian (car date2)))
    ;;         ;;                ))
    ;;         ;;   (< (calendar-absolute-from-gregorian (car date1))
    ;;         ;;      (calendar-absolute-from-gregorian (car date2))))
    ;;   ))

    ;;   (print res))
    ;; (print (holiday-fixed 1 6 "Epiphany"))
    ;; Problem:
    ;; (eval '(((12 28 2024) "Рабочий день")))
    ;; (invalid-function ((12 28 2024) "Рабочий день"))
    ;; Solution: (eval '(list '(12 28 2024) "Рабочий день"))
    ;; ----------- 5
    (mapc 'print (russian-calendar-calendar-holiday-list-slide
            russian-calendar-2024-holidays
            ;; holiday-christian-holidays
            2024
            2024
     ))
    (print (russian-calendar-calendar-holiday-list-slide
            russian-calendar-2024-holidays
            ;; holiday-christian-holidays
            2024
            2024
     ))

    (makunbound 'displayed-month)
    (makunbound 'displayed-year)
    ))


;; (eval ((list '(1 6 2024) "Epiphany")) t)


;; --------- 6
(ert-deftest russian-calendar--calendar-holiday-list ()
  (with-temp-buffer
    (setq displayed-month 2
          displayed-year 2024)
    (let (
          (russian-calendar-2024-holidays
      (mapcar 'purecopy
              '(
                '(((12 28 2024) "Рабочий день2"))
                '(((2 30 2024) "Рабочий день2"))
                '(((12 30 2024) "Вых. Выходной"))
                (holiday-fixed 1 31 "Вых. перед Новый год")
                (holiday-fixed 1 1 "Вых. перед Н")
                )
              ))

          )

      (should (equal (russian-calendar-calendar-holiday-list-slide
                      russian-calendar-2024-holidays
                      ;; holiday-christian-holidays
                      2024
                      2024)
                     '(((1 1 2024) "Вых. перед Н") ((1 31 2024) "Вых. перед Новый год") ((2 30 2024) "Рабочий день2") ((12 28 2024) "Рабочий день2") ((12 30 2024) "Вых. Выходной") ((2 30 2024) "Рабочий день2") ((12 28 2024) "Рабочий день2") ((12 30 2024) "Вых. Выходной") ((2 30 2024) "Рабочий день2") ((12 28 2024) "Рабочий день2") ((12 30 2024) "Вых. Выходной") ((2 30 2024) "Рабочий день2") ((12 28 2024) "Рабочий день2") ((12 30 2024) "Вых. Выходной"))
                     ))
      )
    (makunbound 'displayed-month)
    (makunbound 'displayed-year)
    ))
;; --------- 7 concat

;; (mapc 'print (russian-calendar-concat russian-calendar-2024-open-source-confs
;;                                       russian-calendar-2025-open-source-confs))
;; (mapc 'print russian-calendar-2024-open-source-confs)


(ert-deftest russian-calendar--calendar-holiday-list ()
  (with-temp-buffer
    (setq displayed-month 2
          displayed-year 2024)
      (should (equal (russian-calendar-calendar-holiday-list-slide
                      russian-calendar-2024-open-source-confs
                      2024
                      2024)
                     (russian-calendar-calendar-holiday-list-slide
                      russian-calendar-open-source-confs
                      2024
                      2024)))

    (makunbound 'displayed-month)
    (makunbound 'displayed-year)
    ))
(provide 'russian-calendar-tests)
;;; russian-calendar-tests.el ends here
