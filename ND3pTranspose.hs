{-# LANGUAGE BinaryLiterals#-}
{-# LANGUAGE RebindableSyntax #-}

--module ND3pTranspose where

import Copilot.Arduino.Uno
import qualified Copilot.Arduino.Library.Serial as Serial

import Word8IO

simulationMode :: Bool
simulationMode = True

-- MIDI constants in binary.
-- https://www.midi.org/specifications-old/item/table-1-summary-of-midi-message
noteOn :: Behavior Word8
noteOn = 0b10010000
midiCC :: Behavior Word8
midiCC = 0b10110000

-- Utility functions

counter :: Stream Word8
counter = [1] ++ (1 + counter)

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