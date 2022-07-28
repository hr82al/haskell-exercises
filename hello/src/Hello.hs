module Hello where

sayHello :: String -> IO ()
sayHello word = putStrLn ("Hey " ++ word ++ " my litle cute dogy")
