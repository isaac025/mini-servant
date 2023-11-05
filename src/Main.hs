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

serve :: (HasServer layout) => Proxy layout -> Server layout -> [String] -> IO String
serve p h xs =
    case route p h xs of
        Nothing -> ioError (userError "404")
        Just m -> m

instance (Show a) => HasServer (Get a) where
    route :: Proxy (Get a) -> IO a -> [String] -> Maybe (IO String)
    route _ handler [] = Just (show <$> handler)
    route _ _ _ = Nothing

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
    route ::
        Proxy (a :<|> b) ->
        (Server a :<|> Server b) ->
        [String] ->
        Maybe (IO String)
    route _ (handlera :<|> handlerb) xs =
        route (Proxy :: Proxy a) handlera xs
            <|> route (Proxy :: Proxy b) handlerb xs

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
    route :: Proxy (s :> b) -> Server r -> [String] -> Maybe (IO String)
    route _ handler (x : xs)
        | symbolVal (Proxy :: Proxy s) == x = route (Proxy :: Proxy r) handler xs
    route _ _ _ = Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
    route :: Proxy (Capture a :> b) -> (a -> Server r) -> [String] -> Maybe (IO String)
    route _ handler (x : xs) = do
        a <- readMaybe x
        route (Proxy :: Proxy r) (handler a) xs
    route _ _ _ = Nothing

main :: IO ()
main = putStrLn "Hello, Haskell!"
