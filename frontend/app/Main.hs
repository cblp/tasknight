{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Network.Wai.Handler.Warp (run)

import Tasknight.Frontend (app)

main :: IO ()
main = do
    putStrLn "Start"
    run 3000 app
