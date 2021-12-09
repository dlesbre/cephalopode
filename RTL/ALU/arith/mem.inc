state mem;
actiontype MemRead  = addr:addr -> data:node reading [mem] writing [];
actiontype MemWrite = addr:addr -> data:node -> () reading [mem] writing [mem];
actiontype MemAlloc = addr:addr reading [mem] writing [mem];

// Note: Cephalopode supports simultaneous alloc and read/write, however the ALU verification system doesn't seem to (or, less likely, there is a scheduling bug in Bifrost) so MemAlloc is intentionally entangled with the mem state to prevent simultaneous scheduling.
