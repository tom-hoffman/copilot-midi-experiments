{-# LANGUAGE RebindableSyntax #-}

module ButtonHold where

import Copilot.Arduino.Uno

{- 
This example uses as button and an led.
When you press the button, on the leading edge of the press 
the LED changes state (off to on or vice versa).
This works like a record button (that's what it is for).  
Press once to activate, press again to deactivate.
-}

ledState :: Behavior Bool -> Behavior Bool
-- This function looks at the current and previous states of the button 
-- to determine if it has changed from false to true.  If so, it toggles
-- the state of the LED.
ledState button = v where
    pressed = (button == constant True) && 
              (previous button == constant False)
    new = if pressed then not v else v
    v = [False] ++ new

main :: IO ()
main = arduino $ do
    pullup pin4
    buttonState <- input' pin4 [False, True,  True, True, False, False,
                                False, False, True, True, False, False]
    led =: ledState buttonState
