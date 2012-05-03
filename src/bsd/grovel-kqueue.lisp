;;;; grovel.lisp

(in-package #:cl-fsnotify-kqueue)

(cc-flags "-I/usr/include")

(include "fcntl.h")
(include "sys/types.h")
(include "sys/event.h")
(include "sys/time.h")

(constant (open-read-only        "O_RDONLY"))
(constant (open-non-block        "O_NONBLOCK"))

(constant (kqueue-flag-add       "EV_ADD"))
(constant (kqueue-flag-clear     "EV_CLEAR"))
(constant (kqueue-flag-delete    "EV_DELETE"))

(constant (kqueue-fflag-vnode    "EVFILT_VNODE"))

(constant (kqueue-note-delete    "NOTE_DELETE"))
(constant (kqueue-note-write     "NOTE_WRITE"))
(constant (kqueue-note-extend    "NOTE_EXTEND"))
(constant (kqueue-note-attrib    "NOTE_ATTRIB"))
(constant (kqueue-note-link      "NOTE_LINK"))
(constant (kqueue-note-rename    "NOTE_RENAME"))
(constant (kqueue-note-revoke    "NOTE_REVOKE"))
