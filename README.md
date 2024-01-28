### Description
My WIP implementation of the BitTorrent protocol in Common Lisp.

### Resources
* <http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-1/>
* <http://www.bittorrent.org/beps/bep_0003.html>
* <https://wiki.theory.org/BitTorrentSpecification>

### Setup
Clone the repo in your quicklisp local-projects/ directory, or add a symbolic link pointing to wherever you cloned it.

Then:

```lisp
(ql:quickload 'bittorrent)
```

### Testing
```lisp
(ql:quickload 'bittorrent-test)
(in-package bittorrent-test)
(run! 'bittorrent) ; runs all fiveam tests
```
