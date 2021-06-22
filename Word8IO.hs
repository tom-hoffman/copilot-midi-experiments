{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Word8IO
(writeWord8) where

import Data.Proxy

import Copilot.Arduino.Uno
import qualified Copilot.Arduino.Internals as Internals
import Copilot.Arduino.Library.Serial.Device as Device

instance Input SerialDevice Word8 where
    input' (SerialDevice (SerialDeviceName devname)) interpretvalues =
        Internals.mkInput s
      where
        s = Internals.InputSource
            { Internals.defineVar = Internals.mkCChunk
                [Internals.CLine $ "int " <> varname <> ";"]
            , Internals.setupInput = []
            , Internals.inputPinmode = mempty
            , Internals.readInput = Internals.mkCChunk
                [Internals.CLine $ varname <> " = " <> devname <> ".read();"]
            , Internals.inputStream = extern varname interpretvalues'
            }
        varname = "input_" <> devname
        interpretvalues'
            | null interpretvalues = Nothing
            | otherwise = Just interpretvalues

-- | Write a Word8 to the serial port.
writeWord8 :: Stream Word8 -> FormatOutput
writeWord8 s = FormatOutput
    (Just (arg s))
    (Just (Internals.showCType (Proxy @Word8)))
    (\(Device.SerialDeviceName devname) v ->
        Internals.CLine $ devname <> ".write(" <> v <> ");")
