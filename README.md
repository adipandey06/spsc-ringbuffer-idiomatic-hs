# spsc-ringbuffer-idiomatic-hs
an spsc ringbuffer in idiomatic haskell.

library endpoints exposed:

- data SPSCRingBuffer
    Record representing the ring buffer. Used as a standardized way to refer to the ring buffer.

- initRb :: Word -> IO (SPSCRingBuffer a)
    Method to initialize the ring buffer.

- pushRb :: SPSCRingBuffer a -> a -> IO ()
    Method to write to the ring buffer.

- popRb :: SPSCRingBuffer a -> IO a
    Method to read from the ring buffer.

- destroyRb :: SPSCRingBuffer a -> IO ()
    Method to free resources used by the ring buffer. Does nothing since objects in this implementation live entirely on managed memory.
