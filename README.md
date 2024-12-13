Eng | [Рус](./README.ru.md)

# emacs-russian-calendar

What?
- Russian Production Calendar
- General International holidays: Valentine's Day, April Fools' Day, Halloween.
- Key Orthodox Christian Holidays
- Old Slavic Fests
- Open source conferences: Emacs, FSF, GNU, FOSDEM.
- AI and Russian IT conferences: PyTorh, NeurIPS, IEEE CAI, WAIC, AI Journey dec + TAdviser SummIT nov + CNews Forum nov.
- some common calendar configuration.


Why? Going to be maintained.

Not included: regional holidays.
# 2024 (updated 22/11/24)
# 2025 (updated 22/11/24)
Require updating during the year, some conferences may change.

# Usage

```Elisp
(require 'russian-calendar)

(setopt calendar-holidays (append russian-calendar-holidays
                                  ;; - enable if you need:
                                  ;; russian-calendar-general-holidays
                                  ;; russian-calendar-orthodox-christian-holidays
                                  ;; russian-calendar-old-slavic-fests
                                  ;; russian-calendar-open-source-confs
                                  ;; russian-calendar-ai-confs
                                  ;; russian-calendar-russian-it-confs
                                  ))
;; - optional:
(russian-calendar-localize)
(russian-calendar-set-location-to-moscow)
(russian-calendar-show-diary-holidays-in-calendar)
(russian-calendar-enhance-calendar-movement)
(russian-calendar-fix-list-holidays)
```

# Features
- select current year at loading time
- if year > 2025, then signat that we are obsolate

# Sources
Production Calendar:
- https://www.consultant.ru/law/ref/calendar/proizvodstvennye/2024/
- https://ovodov.me/trud.ics

Emacs and Open source:
- FSF https://www.fsf.org/events/aggregator/
- Emacs https://emacsconf.org/
- GNU https://www.gnu.org/ghm/upcoming.html
- FOSDEM https://fosdem.org
- https://russiaos.ru/#events

AI
- PyTorh https://pytorch.org/events
- NeurIPS https://neurips.cc/
- IEEE CAI https://ieeecai.org/
- WAIC https://www.worldaicannes.com

Russian IT
- AI Journey https://aij.ru/
- https://summit.tadviser.ru/
- https://www.cnconf.ru/

# Resources
Conferences:
- https://developers.sber.ru/kak-v-sbere/events
- https://events.yandex.ru/
- https://meetup.tbank.ru/
- https://opensource.itmo.ru/

Calendars:
- https://github.com/grafov/russian-holidays
- https://github.com/unhammer/calendar-norway.el/blob/master/calendar-norway.el

Orthodox Christian
- https://news.ru/society/samye-vazhnye-cerkovnye-pravoslavnye-prazdniki-v-2022-godu/
- https://religion-info.ru/to-the-saints/12-major-orthodox-holidays-list-the-most-important-orthodox-holidays/
- https://calendar.org.ua/ru/tserkovnyy-kalendar/2024

Old Slavonic
- https://experience.tripster.ru/articles/samye-izvestnye-slavyanskie-prazdniki/
- https://en.wikipedia.org/wiki/Solstice

Regional holidays
- https://normativ.kontur.ru/document?moduleId=41&amp=&documentId=250002
# Other packages
- Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
- Search with Chinese https://github.com/Anoncheg1/pinyin-isearch
- Ediff fix https://github.com/Anoncheg1/ediffnw
- Dired history https://github.com/Anoncheg1/dired-hist

# Screenshot
![](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/cal.png)
