module BankAccount (
  BankAccount
, openAccount
, closeAccount
, getBalance
, incrementBalance
) where

import Data.IORef (IORef, readIORef, newIORef, atomicWriteIORef, atomicModifyIORef')
import Control.Monad (liftM)

type BankAccount = IORef Integer

openAccount :: IO BankAccount
openAccount = newIORef 0

closeAccount :: BankAccount -> IO ()
closeAccount ba = atomicWriteIORef ba 0

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = liftM Just . readIORef

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance ba n = liftM Just $ atomicModifyIORef' ba modifier
  where modifier prev = (prev + n, n)
