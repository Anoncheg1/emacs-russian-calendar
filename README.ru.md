Eng (./README.md) | Рус

# emacs-russian-calendar
Что?
- Производственный календарь
- Несколько междунородных праздников в generals: День св. Валентина, первое апреля, Хеллоуэн.
- Опен сорс главные конференции: Emacs, FSF, GNU, FOSDEM.
- AI и русские IT конференции: PyTorh, NeurIPS, IEEE CAI, WAIC, AI Journey dec + TAdviser SummIT nov + CNews Forum nov.
- обычные настройки календаря, такие как русификация и временная зона.


Зачем? Буду обновлять даты.

# 2024 (обновлено 22/11/24)
# 2025 (обновлено 22/11/24)
Еще буду обновлять даты конференций, производственный календарь скорее всего не измениться, но я проверю перед новым годом.

# Использование

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
;; - необязательные настройки, можешь закоментить:
(russian-calendar-localize)
(russian-calendar-set-location-to-moscow)
(russian-calendar-show-diary-holidays-in-calendar)
(russian-calendar-enhance-calendar-movement)
```

# Ссылки
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

# Resources
Conferences:
- https://developers.sber.ru/kak-v-sbere/events
- https://events.yandex.ru/
- https://meetup.tbank.ru/
- https://opensource.itmo.ru/


Calendars:
- https://github.com/grafov/russian-holidays
- https://github.com/unhammer/calendar-norway.el/blob/master/calendar-norway.el

# Другие мои пакеты
- Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
- Search with Chinese https://github.com/Anoncheg1/pinyin-isearch
- Ediff fix https://github.com/Anoncheg1/ediffnw
- Dired history https://github.com/Anoncheg1/dired-hist

# Screenshot
![](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/cal.png)
