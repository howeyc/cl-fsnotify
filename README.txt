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


Future Plans:
- Expose fsnotify instance so you can have multiple open at a time.
- Figure out what feature to use to get OSX support, as it has same
  kqueue API as BSD systems.
- Windows Support


Note:
Files that are created under a watched directory are automatically
added to the watch list. This includes created directories as well.
