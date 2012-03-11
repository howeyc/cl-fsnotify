cl-fsnotify is created to expose the same file monitoring API
for both Linux, BSD, and possibly (in future) windows.

Copyright (c) 2012 Chris Howey
Released under the ISC License.


Depends On
cl-kqueue  (github.com/howeyc/cl-kqueue)
cl-inotify (github.com/Ferada/cl-inotify)

Working:
BSD
Linux


TODO (Maybe):
Windows


How it works (Also see test.lisp):
(cl-fsnotify:open-fsnotify)
(cl-fsnotify:add-watch "/tmp/blah1")
(cl-fsnotify:add-watch "/tmp/blah1-dir")
....
(cl-fsnotify:get-events)
=> (("/tmp/blah1" . :MODIFY)
    ("/tmp/blah1-dir/new" . :CREATE)
    ("/tmp/blah1-dir/removed" . :DELETE))
...
(cl-fsnotify:close-fsnotify)


Note:
Files that are created under a watched directory are automatically
added to the watch list. This includes created directories as well.
