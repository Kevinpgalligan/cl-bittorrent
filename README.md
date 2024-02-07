### Description
A BitTorrent client in Common Lisp.

The minimum additions to make this actually useful:

* Test it with multi-file torrents.
* Pausing and resuming of downloads.
* Resilience to errors: dropped connections, failed file writes, unresponsive tracker, etc.
* Smarter selection of pieces for download (e.g. rarest first).
* UDP communication with trackers (currently only the old HTTP version).
* Figure out a better way to read bytes from the usocket stream (not one at a time).

### Setup
Clone the repo in your quicklisp local-projects/ directory, or add a symbolic link pointing to wherever you cloned it.

Then:

```lisp
(ql:quickload 'bittorrent)
```

### Usage
```lisp
(in-package bittorrent)
(download-torrent "/path/to/file.torrent" "/path/to/downloads/")
```

### Testing
For unit tests:

```lisp
(ql:quickload 'bittorrent-test)
(in-package bittorrent-test)
(run! 'bittorrent) ; runs all fiveam tests
```

There are multithreading tests in the `scripts/` folder that I run using `slime-eval-buffer` in Emacs.

### Resources
* <http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-1/>
* <http://www.bittorrent.org/beps/bep_0003.html>
* <https://wiki.theory.org/BitTorrentSpecification>
* <http://bittorrent.org/bittorrentecon.pdf>
