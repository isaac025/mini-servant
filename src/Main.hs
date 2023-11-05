module Main where

import Control.Applicative
import Data.Kind
import Data.Time
import GHC.TypeLits
import Text.Read

data Get (a :: Type)

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: Type)
infixr 9 :>

data Capture (a :: Type)

data Proxy a = Proxy

type MyApi =
    "date"
        :> Get Day
        :<|> "time"
        :> Capture TimeZone
        :> Get ZonedTime

type family Server layout :: Type

type instance Server (Get a) = IO a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s :: Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

handleDate :: IO Day
handleDate = utctDay <$> getCurrentTime

handleTime :: TimeZone -> IO ZonedTime
handleTime tz = utcToZonedTime tz <$> getCurrentTime

handleMyApi :: Server MyApi
handleMyApi = handleDate :<|> handleTime

class HasServer layout where
    route :: Proxy layout -> Server layout -> [String] -> Maybe (IO String)

serve :: HasServer layout => Proxy layout -> Server layout -> [String] -> IO String
serve p h xs = 
    case route p h xs of
        Nothing -> ioError (userError "404")
        Just m -> m

    
main :: IO ()
main = putStrLn "Hello, Haskell!"
