{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module CPU.CPU where

import Data.Function (on)
import qualified Data.Word as W (Word8,Word16)
import qualified Data.Vector as V
import Lens.Micro.TH (makeLenses)
import qualified Lens.Micro.Mtl as Lens (view)
import qualified Lens.Micro     as Lens (set)
import qualified System.Random  as Rand

import qualified CPU.Bits as Bits

-- |
-- Modeling a CHIP-8 CPU
data CPU = CPU { _opcode       :: W.Word16
               , _index        :: W.Word16
               , _pc           :: W.Word16
               , _sp           :: W.Word16
               , _delayTimer   :: W.Word8
               , _soundTimer   :: W.Word8
               , _stack        :: V.Vector W.Word16
               , _registers    :: V.Vector W.Word8
               , _keypad       :: V.Vector Bool
               , _memory       :: V.Vector W.Word8
               , _gfx          :: V.Vector Bool
               , _randSeed     :: Rand.StdGen
               } deriving (Read)

instance Eq CPU where
  (==) cpu1 cpu2 =
       test _opcode
    && test _index
    && test _pc
    && test _sp
    && test _delayTimer
    && test _soundTimer
    && test _stack
    && test _registers
    && test _keypad
    && test _memory
    && test _gfx
    && test (show . _randSeed)

      where test f = on (==) f cpu1 cpu2


makeLenses ''CPU



instance Show CPU where
  show cpu = unlines $ map ($ cpu)
    [Bits.showHex16 . _opcode
    ,Bits.showHex16 . _pc
    ,show . _index
    ,show . _delayTimer
    ,show . _soundTimer
    ,show . _sp
    ,show . _stack
    ,show . _registers
    ,show . _keypad
    ,show . _memory
    ,showGfx . _gfx
    ]

-- |
-- initializing the CPU with 0
initCPU :: Rand.StdGen -> CPU
initCPU rgen =
  CPU { _opcode         = 0
      , _index          = 0
      , _pc             = 0
      , _sp             = 0
      , _delayTimer     = 0
      , _soundTimer     = 0
      , _stack          = V.replicate 12 0
      , _registers      = V.replicate 16 0
      , _keypad         = V.replicate 16 False
      , _memory         = V.replicate 4096 0
      , _gfx            = V.replicate (64 * 32) False
      , _randSeed       = rgen
      }

clearKeys :: CPU -> CPU
clearKeys = Lens.set keypad (V.replicate 16 False)

-- |
-- fontset taken from here: http://www.multigesture.net/articles/how-to-write-an-emulator-chip-8-interpreter/
fontSet :: V.Vector W.Word8
fontSet = V.fromList
  [0xF0, 0x90, 0x90, 0x90, 0xF0 -- 0
  ,0x20, 0x60, 0x20, 0x20, 0x70 -- 1
  ,0xF0, 0x10, 0xF0, 0x80, 0xF0 -- 2
  ,0xF0, 0x10, 0xF0, 0x10, 0xF0 -- 3
  ,0x90, 0x90, 0xF0, 0x10, 0x10 -- 4
  ,0xF0, 0x80, 0xF0, 0x10, 0xF0 -- 5
  ,0xF0, 0x80, 0xF0, 0x90, 0xF0 -- 6
  ,0xF0, 0x10, 0x20, 0x40, 0x40 -- 7
  ,0xF0, 0x90, 0xF0, 0x90, 0xF0 -- 8
  ,0xF0, 0x90, 0xF0, 0x10, 0xF0 -- 9
  ,0xF0, 0x90, 0xF0, 0x90, 0x90 -- A
  ,0xE0, 0x90, 0xE0, 0x90, 0xE0 -- B
  ,0xF0, 0x80, 0x80, 0x80, 0xF0 -- C
  ,0xE0, 0x90, 0x90, 0x90, 0xE0 -- D
  ,0xF0, 0x80, 0xF0, 0x80, 0xF0 -- E
  ,0xF0, 0x80, 0xF0, 0x80, 0x80 -- F
  ]

-- |
-- Returns the program counter as Int
getPC :: CPU -> Int
getPC = fromIntegral . Lens.view pc

-- |
-- Returns the stack pointer as Int
getSP :: CPU -> Int
getSP = fromIntegral . Lens.view sp

-- |
-- Returns the content of the register
regVal :: W.Word8 -> CPU -> W.Word8
regVal regNum cpu =
  Lens.view registers cpu V.! fromIntegral regNum

------------
-- Errors
------------

type Error = (Maybe CPU, String)

-- |
-- utility to throw an error
throwErr :: CPU -> String -> Either Error a
throwErr model err = Left (Just model, err)

-- |
-- utility to throw only textual error
throwErrText :: String -> Either Error a
throwErrText = Left . (,) Nothing

showErr :: Error -> String
showErr (Nothing,  err) = "Error: " ++ err
showErr (Just cpu, err) = "Error: " ++ err ++ "\n" ++ show cpu



showGfx :: V.Vector Bool -> String
showGfx = unlines . chunksOf 64 . V.toList . V.map (\t -> if t then 'O' else '-')

-- taken from the package split
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n  = l `c` splitter (drop i l) c n

