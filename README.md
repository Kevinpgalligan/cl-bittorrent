### Description
My implementation of the BitTorrent protocol in Common Lisp.

Nice extensions that I'll probably never do:

* Pausing and resuming of downloads.
* Resilience to e.g. dropped connections, failed file writes, etc.
* Smarter selection of pieces for download (e.g. rarest first).
* UDP communication with trackers.
* Figure out a better way to read bytes from the usocket stream (not one at a time).

### Setup
Clone the repo in your quicklisp local-projects/ directory, or add a symbolic link pointing to wherever you cloned it.

Then:

```lisp
(ql:quickload 'bittorrent)
```

### Usage
TODO

### Testing
For unit tests:

```lisp
(ql:quickload 'bittorrent-test)
(in-package bittorrent-test)
(run! 'bittorrent) ; runs all fiveam tests
```

There are multithreading tests in the `scripts/` folder.

### Resources
* <http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-1/>
* <http://www.bittorrent.org/beps/bep_0003.html>
* <https://wiki.theory.org/BitTorrentSpecification>
* <http://bittorrent.org/bittorrentecon.pdf>