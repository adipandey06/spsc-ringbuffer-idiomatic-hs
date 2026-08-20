# spsc-ringbuffer-idiomatic-hs
an spsc ringbuffer in idiomatic haskell.

library endpoints exposed:

- data SPSCRingBuffer\
  Record representing the ring buffer. Used as a standardized way to refer to the ring buffer.

- initRb :: Word -> IO (SPSCRingBuffer a)\
  Method to initialize the ring buffer.

- tryPushRb :: SPSCRingBuffer a -> a -> IO (Bool)\
  Method to write to the ring buffer.

- tryPopRb :: SPSCRingBuffer a -> IO (Maybe a)\
  Method to read from the ring buffer.

- destroyRb :: SPSCRingBuffer a -> IO ()\
  Method to free resources used by the ring buffer. Does nothing since objects in this implementation live entirely on managed memory.

- getHead :: SPSCRingBuffer a -> IO Word\
  Method that returns the current head of the ring buffer.

- getTail :: SPSCRingBuffer a -> IO Word\
  Method that returns the current tail of the ring buffer.
