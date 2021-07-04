{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

import Copilot.Arduino.Uno

-- Pins corresponding to segments A - G and DP, in order.
segmentConfig = [(pin10 =:),
                 (pin9  =:),
                 (pin7  =:),
                 (pin6  =:),
                 (pin5  =:),
                 (pin11 =:),
                 (pin12 =:),
                 (pin8  =:)]

segments = zip segmentConfig [0 .. 7]

test :: Stream (Array 8 Bool)
test = [array [True, False, True, True, True, True, True, True]] ++ test

setDisplay :: Stream (Array 8 Bool) -> Sketch ()
setDisplay segmentArray =
    mapM_ (\(f, n) -> f (segmentArray .!! constant n)) segments

main :: IO ()
main = arduino $ do
    setDisplay test
    