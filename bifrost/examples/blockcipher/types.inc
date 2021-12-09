type word => "word"; // for counters
type block => "block"; // cipher block
type key => "key"; // cipher key

/* Note: the action types here lie about side-effects, use scissors when needed. */

actiontype ReadFifoBlock = d:block reading [] writing [];
actiontype WriteFifoBlock = d:block -> () reading [] writing [];

actiontype Cipher = decr:bit -> k:key -> din:block -> dout:block reading [] writing []; // encrypt (decr=0) or decrypt (decr=1)
