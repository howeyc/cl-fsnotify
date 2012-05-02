cl-fsnotify is created to expose the same file monitoring API
for both Linux, BSD, and possibly (in future) windows.

Copyright (c) 2012 Chris Howey
Released under the ISC License.


  Supported Common Lisp Implementations and OSs
|===============================================|
|   Common Lisp   |      Operating Systems      |
| Implementations | BSD | OSX | Linux | Windows |
------------------------------------------------|
| CCL             |  Y  |     |   Y   |         |
------------------------------------------------|
| SBCL            |  Y  |     |   Y   |         |
|===============================================|


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


Future Plans:
- Expose fsnotify instance so you can have multiple open at a time.
- Figure out what feature to use to get OSX support, as it has same
  kqueue API as BSD systems.
- Windows Support


Note:
Files that are created under a watched directory are automatically
added to the watch list. This includes created directories as well.
