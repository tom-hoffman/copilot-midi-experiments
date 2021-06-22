{-# LANGUAGE BinaryLiterals#-}
{-# LANGUAGE RebindableSyntax #-}

import Copilot.Arduino.Uno
import qualified Copilot.Arduino.Library.Serial as Serial

import Word8IO

{- 
This is for an Arduino with a SparkFun MIDI board with buttons installed.

  * left  button on pin4.
  * right button on pin2.

When the right button is pressed, the Arduino will trigger a MIDI note to 
each pad in turn.  The ND3p should (if set correctly) output the requested 
note.  The application will read the note played, and return a MIDI CC that
will shift the note up 1 step.  

* wait for the button.
* send a play, noting which pad you're expecting.
* when a note arrives from that pad,
* send a CC with the note value + 1.
* go to the next pad.


The left button follows the same process to shift the note down 1 step.
-}

simulationMode :: Bool
simulationMode = True

-- MIDI constants in binary.
-- https://www.midi.org/specifications-old/item/table-1-summary-of-midi-message
noteOn :: Behavior Word8
noteOn = 0b10010000
midiCC :: Behavior Word8
midiCC = 0b10110000

-- Utility functions

getSerial :: Bool -> Sketch (Behavior Word8)
-- Checks simulationMode and then returns simulated or real data.
getSerial True = input' Serial.device [0, 0b10010000, 2, 0b10010001, 0b10010000]
getSerial False = input Serial.device

isNoteOn :: Behavior Word8 -> Behavior Bool 
isNoteOn serialIn = (serialIn .&. noteOn) == noteOn

main :: IO ()
main = arduino $ do
    Serial.baud 31250
    midiInput <- getSerial simulationMode
    whenB (isNoteOn midiInput) $ Serial.device =: [writeWord8 0b00000001]