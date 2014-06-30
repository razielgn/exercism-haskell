module BankAccount (
  BankAccount
, openAccount
, closeAccount
, getBalance
, incrementBalance
) where

import Data.IORef (IORef, readIORef, newIORef, atomicWriteIORef, atomicModifyIORef')

type BankAccount = IORef (Maybe Integer)

openAccount :: IO BankAccount
openAccount = newIORef (Just 0)

closeAccount :: BankAccount -> IO ()
closeAccount ba = atomicWriteIORef ba Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readIORef

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance ba n = atomicModifyIORef' ba modifier
  where modifier (Just prev) = (Just (prev + n), Just n)
        modifier Nothing     = (Nothing, Nothing)
