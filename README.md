# emacs-russian-calendar
What?
- Russian holidays and Valentine's Day, April Fools' Day, Halloween
- Open source conferences: Emacs, FSF, GNU, FOSDEM
- AI and Russian IT conferences: PyTorh, NeurIPS, IEEE CAI, WAIC, AI Journey dec + TAdviser SummIT nov + CNews Forum nov
- some common calendar configuration
Why? Going to be maintained.

# 2024 (updated 22/11/24)
# 2025 (updated 22/11/24)
- require updating during the year: Emacs, FSF, GNU, https://nrussiaos.ru/#events
- NeurIPS 10 dec, but may change.
- AI Journey 11 dec, but may chage.
- EmacsConf somewhere in dec.

# Usage

```Elisp
(require 'calendar)
(require 'holidays)
(require 'russian-calendar)

(setopt calendar-holidays (append russian-calendar-holidays
                                  russian-calendar-general-holidays
                                  ;; - enable if you need:
                                  ;; russian-calendar-open-source-confs
                                  ;; russian-calendar-ai-confs
                                  ;; russian-calendar-russian-it-confs
                                  ))
;; - optional:
(russian-calendar-localize)
(russian-calendar-set-location-to-moscow)
(russian-calendar-show-diary-holidays-in-calendar)
(russian-calendar-enhance-calendar-movement)
```

# sources
Russian holidays:
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

# resources
Conferences:
- https://developers.sber.ru/kak-v-sbere/events
- https://events.yandex.ru/
- https://meetup.tbank.ru/
- https://opensource.itmo.ru/
Calendars:
- https://github.com/grafov/russian-holidays
- https://github.com/unhammer/calendar-norway.el/blob/master/calendar-norway.el

# other packages
- Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
- Search with Chinese https://github.com/Anoncheg1/pinyin-isearch
- Ediff fix https://github.com/Anoncheg1/ediffnw
- Dired history https://github.com/Anoncheg1/dired-hist
