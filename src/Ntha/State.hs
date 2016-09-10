module Ntha.State where

import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE createState #-}
createState :: a -> IORef a
createState = unsafePerformIO . newIORef

{-# NOINLINE readState #-}
readState :: IORef a -> a
readState = unsafePerformIO . readIORef

type Infer a = IO a

currentId :: IORef Int
currentId = createState 0

nextId :: Infer Int
nextId = do
    v <- readIORef currentId
    writeIORef currentId (v + 1)
    return v

resetId :: Infer ()
resetId = writeIORef currentId 0

currentUniqueName :: IORef Char
currentUniqueName = createState 'α'

nextUniqueName :: Infer String
nextUniqueName = do
    char <- readIORef currentUniqueName
    if char == 'ω'
    then resetUniqueName
    else writeIORef currentUniqueName $ succ char
    return [char]

resetUniqueName :: Infer ()
resetUniqueName = writeIORef currentUniqueName 'α'
