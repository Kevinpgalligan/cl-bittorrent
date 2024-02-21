### Description
A BitTorrent client in Common Lisp.

The minimum additions to make this actually useful:

* Pausing and resuming of downloads.
* Ability to pick which files to download.
* Resilience to errors: dropped connections, failed file writes, unresponsive tracker, etc.
* Smarter selection of pieces for download (e.g. rarest first).
* UDP communication with trackers (currently uses the old HTTP protocol, which most trackers don't accept anymore).
* Let the tracker know when we've finished / are shutting down.
* Handle case where client is in tracker's list of peers, i.e. don't try communicating with self.

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
Unit tests:

```lisp
(ql:quickload 'bittorrent-test)
(in-package bittorrent-test)
(run! 'bittorrent) ; runs all fiveam tests
```

Integration tests in the `scripts/` folder:

* `handshake-test.lisp` is used to test the handshake / message-sending between two peer threads. Run in Emacs with `slime-eval-buffer`.
* `tracker.lisp` is used to construct and save a dummy tracker response. Set `*peer*` if you want a peer listed in the response, run `(save-tracker-response)` to save the response at the appropriate path.
* `tracker.py` runs a dummy tracker server that returns the response generated by `tracker.lisp`.
* `run-client` runs a fresh instance of the client with no existing data, accepts as arguments the path to the torrent file and the path to the download directory. Run from the command-line.

To test 1 client with no peers: (1) start the dummy tracker, (2) run a client from the REPL using `download-torrent`.

To test 2 clients talking to each other:

0. Save tracker response to /tmp/ with `scripts/tracker.lisp`.
1. Start the dummy tracker (`python3 scripts/tracker.py`).
2. Run a client from the command-line (with `scripts/run-client` and the `for-tests.torrent` file).
3. Add that client's info (port and ID, from the logs) to the tracker. Note that the client's ID is URL-encoded and you will want to decode it using `(quri:url-decode "STRING-HERE" :encoding :latin1)`.
4. Run another client from the REPL using `download-torrent`, this time include a bit vector as an extra argument indicating which pieces it has downloaded (in the test I'm doing, one client has all the data and the other has none). Example invocation: `(download-torrent "/path/to/cl-bittorrent/data/for-tests.torrent" "/path/to/downloaded/file/" (make-array 901 :element-type 'bit :initial-element 1))`.

### References
* <http://www.kristenwidman.com/blog/how-to-write-a-bittorrent-client-part-1/>
* <http://www.bittorrent.org/beps/bep_0003.html>
* <https://wiki.theory.org/BitTorrentSpecification>
* <http://bittorrent.org/bittorrentecon.pdf>