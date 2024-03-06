### Choking logic
Here's my understanding of the choking/unchoking logic in the spec. It does not
reflect my implementation.

DOWNLOADING:
Step 1: Get 4 downloaders ("top uploaders who are interested").
Step 2: Any uninterested peers with a higher upload rate than those downloaders
        gets unchoked too.
        What if they don't declare their interest but still request pieces?
        Do we drop their requests?
Step 3: If those uninterested folks become interested, then the worst of the
        downloaders gets choked.

UPLOADING:
Pick 4 peers who are downloading from us at the highest rate, who are
interested.

Should also weigh newly-connected peers higher in the optimistic unchoking, to give
them a chance to acquire a full piece.

### Client logic
0. Init general state.
   - bit vector for pieces.
   - set directory.
1. Contact tracker.
2. Init per-peer state.
   - what pieces they have
   - choked/unchoked, interested/uninterested
   - uploaded bytes, downloaded bytes (to calculate ratio).
   - time since last ping.
   - ?? pending requests ??
3. Spin up threads for each of the peers.
4. Start loop.
   a. If time expired, recalculate choked/whatever and send updates.
   b. Process messages.
        choke -> update our state.
        unchoke -> "
        interested -> "
        uninterested -> "
        have -> if they now have a piece that we're interested in, express
                that interest.
        bitfield -> update our state.
        request -> if they're choked, drop it.
                   if request size too big, drop it.
                   if not, actually give them the block.
                   may require rate-limiting at some point.
        piece -> assuming we requested it, accumulate.
                 when a piece is completed, check if we are still interested
                 in other clients, and if not, send uninterested.
   c. Make requests to peers who have unchoked us and who have pieces we want.
      Up to 5 concurrent requests. So need to track ongoing requests.
   d. Check if keepalive needed.
5. When completed, update the tracker.
