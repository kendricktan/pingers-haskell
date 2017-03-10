module Main where

import Control.Monad        (msum)
import Data.Char            (toLower)
import Happstack.Server     ( Method(GET, POST), FromReqURI(..)
                            , nullConf, simpleHTTP, method
                            , toResponse, path, ok, dir, seeOther
                            )

data Subject = World | Haskell


sayHello :: Subject -> String
sayHello World   = "Hello World"
sayHello Haskell = "Hello Haskell"


instance FromReqURI Subject where
    fromReqURI sub =
        case map toLower sub of
            "haskell" -> Just Haskell
            "world"   -> Just World
            _         -> Nothing


main :: IO ()
main = simpleHTTP nullConf $ msum
    [ dir "hello"   $ path $ \s -> do method GET
                                      ok (sayHello s)
    , dir "hello"   $ path $ \s -> do method POST
                                      ok ("You posted, " ++ s)
    , dir "goodbye" $ ok "Goodbye World!"
    , do method POST
         ok "You did a POST request"
    , seeOther "/goodbye" "/goodbye"
    ]
