-- Using Idiomatic Haskell (Vectors + IORefs)

module RingBuffer.SPSC.Idiomatic (
    SPSCRingBuffer,
    initRb,
    pushRb,
    popRb,
    destroyRb,
    getHead,
    getTail
) where

import Data.Vector.Mutable as MV
import Data.IORef


data SPSCRingBuffer a = SPSCRingBuffer {
    buffer :: IOVector a,
    capacity :: Word,
    tail :: IORef Word,
    head :: IORef Word
}

initRb :: Word -> IO (SPSCRingBuffer a)
initRb capacity = do
    buffer <- MV.new (fromIntegral capacity)
    readIndex <- newIORef 0
    writeIndex <- newIORef 0
    return $ SPSCRingBuffer buffer capacity readIndex writeIndex

-- methods to readRb/writeRb the readRb/writeRb positions.
getHead :: SPSCRingBuffer a -> IO Word
getHead rb = readIORef (head rb)

setHead :: SPSCRingBuffer a -> Word -> IO ()
setHead rb pos = writeIORef (head rb) pos

getTail :: SPSCRingBuffer a -> IO Word
getTail rb = readIORef (tail rb)

setTail :: SPSCRingBuffer a -> Word -> IO ()
setTail rb pos = writeIORef (tail rb) pos


-- helper method to return the wrap-incremented value of a pointer, given the capacity of the ring buffer and current position
wrapIncrement :: Word -> Word -> Word
wrapIncrement pos capacity = (pos + 1) `mod` capacity


-- DEPRECATED

-- -- spin-waiting methods
-- spinUntilSpace :: SPSCRingBuffer a -> Word -> IO ()
-- spinUntilSpace rb wp = do
--     rp <- getPopPos rb
--     if (wrapIncrement wp (capacity rb)) == rp
--         then spinUntilSpace rb wp
--         else return ()

-- spinUntilNotEmpty :: SPSCRingBuffer a -> Word -> IO ()
-- spinUntilNotEmpty rb rp = do
--     wp <- getPushPos rb
--     if rp == wp
--         then spinUntilNotEmpty rb rp
--         else return ()

-- popRb :: SPSCRingBuffer a -> IO a
-- popRb rb = do
--     -- 1. Get the tail pointer.
--     readPos <- getPopPos rb

--     -- 2. If the slot to be read is the same as the next free slot, that means that the buffer is empty.
--     spinUntilNotEmpty rb readPos

--     -- 3. get the item at the (to be) read position
--     item <- MV.read (buffer rb) (fromIntegral readPos)

--     -- 4. Increment the tail (read) pointer, wrapping around if necessary
--     let nextReadPos = wrapIncrement readPos (capacity rb)
--     setPopPos rb nextReadPos

--     return item

-- pushRb :: SPSCRingBuffer a -> a -> IO ()
-- pushRb rb item = do
--     -- 1. Get the head pointer. 
--     writePos <- getPushPos rb
    
--     -- 2. Checks if it's safe to pushRb to nextWp (the current head pointer) by checking if it's not equal to the tail (read) pointer. if it is, then it hasn't been read yet, meaning it's not safe to pushRb on without corrupting/dropping data.
--     spinUntilSpace rb writePos 

--     -- 3. Write the item to the head pointer
--     MV.write (buffer rb) (fromIntegral writePos) item

--     -- 4. Increment the head pointer, wrapping around if necessary
--     let nextWritePos = wrapIncrement writePos (capacity rb)
--     setPushPos rb nextWritePos

-- DEPRECATED


isFull :: SPSCRingBuffer a -> Word -> IO Bool
isFull rb head= do
    tail <- getTail rb
    return $ (wrapIncrement head (capacity rb)) == tail

tryPushRb :: SPSCRingBuffer a -> a -> IO Bool
tryPushRb rb item = do
    -- 1. Get the head pointer.
    head <- getHead rb

    -- 2. Check if the buffer is full
    full <- isFull rb head
    if full
        then return False
        else do
            -- 3. Write the item to the head pointer
            MV.write (buffer rb) (fromIntegral head) item

            -- 4. Increment the head pointer, wrapping around if necessary
            let nextHead = wrapIncrement head (capacity rb)
            setHead rb nextHead
            return True



isEmpty :: SPSCRingBuffer a -> Word -> IO Bool
isEmpty rb tail = do
    head <- getHead rb
    return $ tail == head

tryPopRb :: SPSCRingBuffer a -> IO (Maybe a)
tryPopRb rb = do
    -- 1. Get the tail pointer.
    tail <- getTail rb

    -- 2. Check if the buffer is empty
    empty <- isEmpty rb tail
    if empty
        then return Nothing
        else do
            -- 3. Get the item at the (to be) read position
            item <- MV.read (buffer rb) (fromIntegral tail)

            -- 4. Increment the tail (read) pointer, wrapping around if necessary
            let nextTail = wrapIncrement tail (capacity rb)
            setTail rb nextTail
            return (Just item)
    

destroyRb :: SPSCRingBuffer a -> IO ()
destroyRb rb = do
    -- In this implementation, we don't have any explicit resources to free, as the garbage collector will take care of the mutable vector and IORefs. this function does nothing.
    return()