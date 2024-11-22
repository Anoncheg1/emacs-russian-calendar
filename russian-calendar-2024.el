;;; russian-calendar-2024.el --- Holidays and conferences -*- lexical-binding: t -*-


;;; Commentary:
;;

;;; Code:

(defvaralias 'russian-calendar-holidays 'russian-calendar-2024-holidays)
(defvaralias 'russian-calendar-general-holidays 'russian-calendar-2024-general-holidays)
(defvaralias 'russian-calendar-open-source-confs 'russian-calendar-2024-open-source-confs)
(defvaralias 'russian-calendar-ai-confs 'russian-calendar-2024-ai-confs)


(defvar russian-calendar-2024-holidays
  '(
    (holiday-fixed 1 1 "Новогодние каникулы")
    (holiday-fixed 1 2 "Новогодние каникулы")
    (holiday-fixed 1 3 "Новогодние каникулы")
    (holiday-fixed 1 4 "Новогодние каникулы")
    (holiday-fixed 1 5 "Новогодние каникулы")
    (holiday-fixed 1 8 "Новогодние каникулы")
    (holiday-fixed 1 7 "Рождество Христово")
    (holiday-fixed 2 22 "* Сокращённый день")
    (holiday-fixed 2 23 "День защитника Отечества")
    (holiday-fixed 3 7 "* Сокращённый день")
    (holiday-fixed 3 8 "Международный женский день")
    (holiday-fixed 4 27 "Work day")
    (holiday-fixed 4 29 "May holiday")
    (holiday-fixed 4 30 "May holiday")
    (holiday-fixed 5 1 "Праздник Весны и Труда")
    (holiday-fixed 5 8 "* Сокращённый день")
    (holiday-fixed 5 9 "День Победы")
    (holiday-fixed 5 10 "Выходные")
    (holiday-fixed 6 11 "* Сокращённый день")
    (holiday-fixed 6 12 "День России")
    (holiday-fixed 11 4 "День народного единства")
    (holiday-fixed 11 2 "* Сокращённый день")
    (holiday-fixed 12 28 "Work day")
    (holiday-fixed 12 30 "Weekend day")
    (holiday-fixed 12 31 "Weekend day")
    ))

(defvar russian-calendar-2024-general-holidays
  '(
    (holiday-fixed 2 14 "Valentine's Day")
    (holiday-fixed 4 1 "April Fools' Day")
    (holiday-fixed 10 31 "Halloween")
    ))

(defvar russian-calendar-2024-open-source-confs
  '(
    (holiday-fixed 2 3 "FOSDEM https://fosdem.org")
    (holiday-fixed 12 7 "EmacsConf https://emacsconf.org/")
    ))
(defvar russian-calendar-2024-ai-confs
  '(
    (holiday-fixed 11 28 "TAdviser SummIT https://tadvisersummit.ru/")
    (holiday-fixed 12 10 "NeurIPS https://neurips.cc/")
    (holiday-fixed 12 11 "AI Journey https://aij.ru/")
    ))

(provide 'russian-calendar-2024)
;;; russian-calendar-2024.el ends here
