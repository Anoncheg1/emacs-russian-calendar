# emacs-russian-calendar
- Russian holidays and Valentine's Day, April Fools' Day, Halloween
- Open source conferences: Emacs, FSF, GNU, FOSDEM
- AI conferences: PyTorh, NeurIPS, IEEE CAI, WAIC, AI Journey dec + TAdviser SummIT nov + CNews Forum nov

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
(require 'russian-calendar-2024)

(setopt diary-show-holidays-flag t)
(setopt calendar-mark-holidays-flag t)
(setopt calendar-week-start-day 1)

(setopt calendar-holidays (append russian-calendar-holidays
                                  russian-calendar-general-holidays
                                  russian-calendar-open-source-confs
                                  russian-calendar-ai-confs))
```

# sources
Russian holidays:
- https://ovodov.me/trud.ics
- https://www.consultant.ru/law/ref/calendar/proizvodstvennye/2024/

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
- AI Journey https://aij.ru/

# resources
- https://github.com/grafov/russian-holidays
- https://developers.sber.ru/kak-v-sbere/events
- https://events.yandex.ru/
- https://tadviser.com/index.php/Company:TAdviser
- https://www.cnconf.ru/
- https://meetup.tbank.ru/
