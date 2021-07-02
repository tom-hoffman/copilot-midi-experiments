{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}

import Copilot.Arduino.Uno

segA =  pin10
segB =  pin9
segC =  pin7
segD =  pin6
segE =  pin5
segF =  pin11
segG =  pin12
segDP = pin8

setDisplay :: Stream (Array 8 Bool) -> Sketch ()
setDisplay pins = do
    segA =: pins .!! constant 0
    segB =: pins .!! constant 1
    segC =: pins .!! constant 2
    segD =: pins .!! constant 3
    segE=: pins .!! constant 4
    segF =: pins .!! constant 5
    segG=: pins .!! constant 6
    segDP =: pins .!! constant 7

test :: Stream (Array 8 Bool)
test = [array [True, False, True, True, True, True, True, True]] ++ test

main :: IO ()
main = arduino $ do
    setDisplay test

arr :: Stream (Array 2 Bool)
arr = [ array [True, False]
      , array [True, True]
      , array [False, False]] ++ arr