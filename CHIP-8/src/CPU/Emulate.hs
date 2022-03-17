{-# LANGUAGE LambdaCase #-}

module CPU.Emulate where

-------------
-- Imports
-------------

import Control.Monad ((>=>))
import qualified Data.Word as W (Word8, Word16)
import qualified Data.Vector as V
import Data.Bits ((.&.), (.|.), xor, shiftR, shiftL)
import Lens.Micro ((&))
import qualified Lens.Micro     as Lens
import qualified Lens.Micro.Mtl as Lens
import qualified Data.ByteString as BS
import qualified System.Random as Rand

import CPU.Utils (supplyBoth)
import CPU.CPU (CPU, Error, throwErr, throwErrText)
import qualified CPU.CPU as CPU
import qualified CPU.Bits as Bits

-------------
-- Loading
-------------

loadGameAndFonts :: Rand.StdGen -> BS.ByteString -> Emulate CPU
loadGameAndFonts rgen =
  loadGame rgen >=> pure . loadFonts

loadGame :: Rand.StdGen -> BS.ByteString -> Emulate CPU
loadGame rgen game =
  if BS.length game <= V.length (Lens.view CPU.memory cpu) - 0x200
  then
    pure $ cpu & Lens.set  CPU.pc 0x200
               & Lens.over CPU.memory (`V.update` V.fromList (zip [0x200..] (BS.unpack game)))
  else
    throwErrText "Cannot fit game in memory"
  where cpu = CPU.initCPU rgen

loadFonts :: CPU -> CPU
loadFonts =
  Lens.over CPU.memory (\mem -> V.update_ mem (V.enumFromN 0 (V.length CPU.fontSet)) CPU.fontSet)

---------------
-- Emulation
---------------

type Emulate a = Either Error a
type Instruction = (CPU -> Emulate CPU)

withDefault :: Maybe a -> Emulate a -> Emulate a
withDefault (Just x) _ = pure x
withDefault _ eDefault = eDefault

emulateCycle :: CPU -> Emulate CPU
emulateCycle cpu = execute cpu =<< decode cpu =<< fetch cpu

fetch :: CPU -> Either Error W.Word16
fetch cpu =
  case cmd of
    Just instruction ->
      pure instruction
    Nothing ->
      throwErr cpu "Program counter out of bounds"

  where cmd = Bits.merge16 <$> (Lens.view CPU.memory cpu V.!? CPU.getPC cpu)
                           <*> (Lens.view CPU.memory cpu V.!? (CPU.getPC cpu + 1))

decode :: CPU -> W.Word16 -> Emulate Instruction
decode cpu cmd =
 case findOpcode cmd of
    Just instruction ->
      pure instruction
    Nothing ->
      throwErr cpu $ "Opcode: " ++ Bits.showHex16 cmd ++ " not implemented."


execute :: a -> (a -> b) -> b
execute = flip ($)

updateTimers :: CPU -> CPU
updateTimers cpu =
  cpu & Lens.over CPU.delayTimer (\dt -> if dt > 0 then dt - 1 else 0)
      & Lens.over CPU.soundTimer (\st -> if st > 0 then st - 1 else 0)

cleanSoundTimer :: CPU -> CPU
cleanSoundTimer =
   Lens.set CPU.soundTimer 0

----------------
-- update CPU
----------------

-- |
-- increases the program counter by 2
nextPC :: CPU -> CPU
nextPC =
  Lens.over CPU.pc (+2)

-- |
-- store a value on the stack, updating the stack and the stack pointer.
-- fail if out of bounds
storeOnStack :: W.Word16 -> CPU -> Emulate CPU
storeOnStack val cpu =
  if CPU.getSP cpu + 1 >= V.length (Lens.view CPU.stack cpu)
  then throwErr cpu "The stack is full"
  else pure $
    cpu & Lens.over CPU.stack (V.// [(CPU.getSP cpu + 1, val)])
        & Lens.over CPU.sp (+1)

-- |
-- pops a value from the stack, updating the stack pointer.
-- fail if out of bounds
popFromStack :: CPU -> Emulate (W.Word16, CPU)
popFromStack cpu =
  if CPU.getSP cpu <= 0 && CPU.getSP cpu >= V.length (Lens.view CPU.stack cpu)
  then
    throwErr cpu "The stack is empty"
  else
    pure (Lens.view CPU.stack cpu V.! CPU.getSP cpu, Lens.over CPU.sp (\x -> x-1) cpu)


-------------
-- Opcodes
-------------

-- |
-- finds the relevant instruction from opcode.
-- Based on this: https://en.wikipedia.org/wiki/CHIP-8#Opcode_table
findOpcode :: W.Word16 -> Maybe Instruction
findOpcode opcode =
  case Bits.match16 opcode of
    (0x0, 0x0, 0x0, 0x0) ->
      pure $ \cpu -> throwErr cpu "DUMP"
    (0x0, 0x0, 0xE, 0x0) ->
      pure $ pure . nextPC >=> clearScreen
    (0x0, 0x0, 0xE, 0xE) ->
      -- "Returns from a subroutine."
      pure $ pure . nextPC >=> returnFromSubroutine
    (0x0, _, _, _) ->
      Nothing -- "Calls RCA 1802 program at address NNN. Not necessary for most ROMs." - not yet implemented
    (0x1, _, _, _) ->
      pure $ jump (opcode .&. 0x0FFF)
    (0x2, _, _, _) ->
      pure $ pure . nextPC >=> callSubroutine (opcode .&. 0x0FFF)
    (0x3, reg, _, _) ->
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> pure $ CPU.regVal reg cpu == fromIntegral (opcode .&. 0x00FF))
    (0x4, reg, _, _) ->
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> pure $ CPU.regVal reg cpu /= fromIntegral (opcode .&. 0x00FF))
    (0x5, reg1, reg2, 0x0) ->
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> pure $ CPU.regVal reg1 cpu == CPU.regVal reg2 cpu)
    (0x6, v, _, _) ->
      pure $ pure . nextPC >=> setRegister v (fromIntegral (opcode .&. 0x00FF))
    (0x7, v, _, _) ->
      pure $ pure . nextPC >=> addToRegister v (fromIntegral (opcode .&. 0x00FF))
    (0x8, x, y, 0x0) ->
      pure $ pure . nextPC >=> movRegister x y
    (0x8, x, y, 0x1) ->
      pure $ pure . nextPC >=> orRegisters x y
    (0x8, x, y, 0x2) ->
      pure $ pure . nextPC >=> andRegisters x y
    (0x8, x, y, 0x3) ->
      pure $ pure . nextPC >=> xorRegisters x y
    (0x8, x, y, 0x4) ->
      pure $ pure . nextPC >=> addRegisters x y
    (0x8, x, y, 0x5) ->
      pure $ pure . nextPC >=> subRegisters x y
    (0x8, x, _, 0x6) ->
      pure $ pure . nextPC >=> shiftRegisterR x
    (0x8, x, y, 0x7) ->
      pure $ pure . nextPC >=> subRegistersBackwards x y
    (0x8, x, _, 0xE) ->
      pure $ pure . nextPC >=> shiftRegisterL x
    (0x9, reg1, reg2, 0x0) ->
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> pure $ CPU.regVal reg1 cpu /= CPU.regVal reg2 cpu)
    (0xA, _, _, _) ->
      pure $ pure . nextPC >=> setIndex (opcode .&. 0x0FFF)
    (0xB, _, _, _) ->
      pure $ jumpPlusIndex (opcode .&. 0x0FFF)
    (0xC, reg, _, _) ->
      -- "Sets VX to the result of a bitwise and operation on a random number and NN." -- not yet implemented
      pure $ pure . nextPC >=> setRegWithRand (fromIntegral opcode .&. 0xFF) reg
    (0xD, reg1, reg2, times) ->
      -- "Sprites stored in memory at location in index register (I), 8bits wide. Wraps around the screen. If when drawn, clears a pixel, register VF is set to 1 otherwise it is zero. All drawing is XOR drawing (i.e. it toggles the screen pixels). Sprites are drawn starting at position VX, VY. N is the number of 8bit rows that need to be drawn. If N is greater than 1, second line continues at position VX, VY+1, and so on." -- not yet implemented
      pure $ pure . nextPC >=> drawSprite reg1 reg2 (fromIntegral times)
    (0xE, reg, 0x9, 0xE) ->
      -- "Skips the next instruction if the key stored in VX is pressed."
      pure $ pure . nextPC >=> skipInstructionIf (isKey id reg)
    (0xE, reg, 0xA, 0x1) ->
      -- "Skips the next instruction if the key stored in VX isn't pressed."
      pure $ pure . nextPC >=> skipInstructionIf (isKey not reg)
    (0xF, reg, 0x0, 0x7) ->
      pure $ pure . nextPC >=> \cpu -> setRegister reg (Lens.view CPU.delayTimer cpu) cpu
    (0xF, reg, 0x0, 0xA) ->
      -- "A key press is awaited, and then stored in VX."
      pure $ waitForKey reg
    (0xF, reg, 0x1, 0x5) ->
      pure $ pure . nextPC >=> \cpu -> pure $ Lens.set CPU.delayTimer (CPU.regVal reg cpu) cpu
    (0xF, reg, 0x1, 0x8) ->
      pure $ pure . nextPC >=> \cpu -> pure $ Lens.set CPU.soundTimer (CPU.regVal reg cpu) cpu
    (0xF, reg, 0x1, 0xE) ->
      pure $ pure . nextPC >=> \cpu -> setIndex (fromIntegral (CPU.regVal reg cpu) + Lens.view CPU.index cpu) cpu
    (0xF, reg, 0x2, 0x9) ->
      -- "Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font."
      pure $ pure . nextPC >=> \cpu -> pure $ Lens.set CPU.index (fromIntegral (CPU.regVal reg cpu) * 5) cpu
    (0xF, reg, 0x3, 0x3) ->
      pure $ pure . nextPC >=> storeBinRep reg
    (0xF, reg, 0x5, 0x5) ->
      pure $ pure . nextPC >=> storeRegInMemory reg
    (0xF, reg, 0x6, 0x5) ->
      pure $ pure . nextPC >=> storeMemoryInReg reg
    _ -> Nothing -- Unrecognized opcode



-- |
-- Opcode 0x00E0
-- Clears the screen
clearScreen :: Instruction
clearScreen =
  pure . Lens.over CPU.gfx (V.map (const False))

-- |
-- Opcode 0x1nnn
-- Jump to address 0x0nnn
-- changes the program counter to given address
jump :: W.Word16 -> Instruction
jump address =
  pure . Lens.set CPU.pc address

-- |
-- Opcode 0x2nnn
-- Call a subroutine on address 0x0nnn
-- Saves the program counter on the stack and changes it to given address
callSubroutine :: W.Word16 -> Instruction
callSubroutine address =
  supplyBoth storeOnStack (Lens.view CPU.pc) >=> jump address

returnFromSubroutine :: Instruction
returnFromSubroutine cpu = do
  (pc, cpu') <- popFromStack cpu
  pure $ Lens.set CPU.pc pc cpu'

-- |
-- Opcodes 0x3vnn, 0x4vnn, 0x5xy0, 0x9xy0
-- Skips instruction if (test cpu) is true
-- may change the program counter (pc)
skipInstructionIf :: (CPU -> Emulate Bool) -> Instruction
skipInstructionIf test cpu =
  test cpu >>= \case
    True ->
      pure $ nextPC cpu
    False ->
      pure cpu


-- |
-- Opcode 0xBnnn
-- Jump to address 0x0nnn + content of index register
-- changes the program counter
jumpPlusIndex :: W.Word16 -> Instruction
jumpPlusIndex address cpu =
  jump (address + Lens.view CPU.index cpu) cpu

-- |
-- Opcode 0xFx33
-- Write the binary-coded decimal of register x to memory
-- at addresses index, index+1 and index+2
-- Changes the memory
storeBinRep :: W.Word8 -> Instruction
storeBinRep x cpu =
  let
    (n1,n2,n3) = Bits.bcd8 $ Lens.view CPU.registers cpu V.! fromIntegral x
    (i1,i2,i3) = (\i -> (fromIntegral i, fromIntegral i+1, fromIntegral i+2)) (Lens.view CPU.index cpu)
 in
    pure $ Lens.over CPU.memory (V.// [(i1, n1), (i2, n2), (i3, n3)]) cpu


-- |
-- Opcode 0xAnnn
-- Sets the index register to the immediate value nnn
-- changes the index register
setIndex :: W.Word16 -> Instruction
setIndex address =
  pure . Lens.set CPU.index address


-- |
-- Opcode 0x6vnn
-- Sets the register v to the immediate value nn
-- changes the register v
setRegister :: W.Word8 -> W.Word8 -> Instruction
setRegister regNum value =
  pure . Lens.over CPU.registers (V.// [(fromIntegral regNum, value)])

-- |
-- Opcode 0x7vnn
-- Add the immediate value nn to the register v
-- changes the register v
addToRegister :: W.Word8 -> W.Word8 -> Instruction
addToRegister regNum value cpu =
  setRegister regNum (value + vx) cpu
  where vx = CPU.regVal regNum cpu


-- |
-- Opcode 0x8xy0
-- Sets the register x to the content of register y
-- changes the register x
movRegister :: W.Word8 -> W.Word8 -> Instruction
movRegister = binOpRegisters (flip const)

-- |
-- Opcode 0x8xy1
-- Sets the register x to (x | y)
-- changes the register x
orRegisters :: W.Word8 -> W.Word8 -> Instruction
orRegisters = binOpRegisters (.|.)


-- |
-- Opcode 0x8xy2
-- Sets the register x to (x & y)
-- changes the register x
andRegisters :: W.Word8 -> W.Word8 -> Instruction
andRegisters = binOpRegisters (.&.)

-- |
-- Opcode 0x8xy3
-- Sets the register x to (x `xor` y)
-- changes the register x
xorRegisters :: W.Word8 -> W.Word8 -> Instruction
xorRegisters = binOpRegisters xor


-- |
-- Opcode 0x8xy4
-- Adds the registers x and y and stores them at register x. sets carry in register F
-- changes the registers x and F
addRegisters :: W.Word8 -> W.Word8 -> Instruction
addRegisters x y cpu =
  pure $ Lens.over CPU.registers (V.// [(fromIntegral x, vx + vy), (0xF, carry)]) cpu
  where vx = CPU.regVal x cpu
        vy = CPU.regVal y cpu
        carry = if (0xFF :: W.Word16) < fromIntegral vx + fromIntegral vy then 1 else 0

-- |
-- Opcode 0x8xy5
-- Subtract the registers y from x and store in x. sets borrow in register F
-- changes the registers x and F
subRegisters :: W.Word8 -> W.Word8 -> Instruction
subRegisters x y cpu =
  pure $ Lens.over CPU.registers (V.// [(fromIntegral x, vx - vy), (0xF, borrow)]) cpu
  where vx = CPU.regVal x cpu
        vy = CPU.regVal y cpu
        borrow = if vx < vy then 0 else 1

-- |
-- Opcode 0x8xy7
-- Subtract the registers x from y and store in x. sets borrow in register F
-- changes the registers x and F
subRegistersBackwards :: W.Word8 -> W.Word8 -> Instruction
subRegistersBackwards x y cpu =
  pure $ Lens.over CPU.registers (V.// [(fromIntegral x, vy - vx), (0xF, borrow)]) cpu
  where vx = CPU.regVal x cpu
        vy = CPU.regVal y cpu
        borrow = if vx > vy then 0 else 1


-- |
-- Opcode 0x8x_6
-- Shifts register x right by 1 and sets register F to the value
-- of the LSB of reg x before the shift
shiftRegisterR :: W.Word8 -> Instruction
shiftRegisterR = shiftRegister shiftR comp
    where comp reg = reg .&. 0x1


-- |
-- Opcode 0x8x_E
-- Shifts register x left by 1 and sets register F to the value
-- of the MSB of reg x before the shift
shiftRegisterL :: W.Word8 -> Instruction
shiftRegisterL = shiftRegister shiftL comp
    where comp reg = shiftR (reg .&. 0x80) 7


-- |
-- Shifts register x by 1 and sets register F to the value
-- of a significant bit of reg x before the shift
-- changes the registers x and F
shiftRegister :: (W.Word8 -> Int -> W.Word8) -> (W.Word8 -> W.Word8) -> W.Word8 -> Instruction
shiftRegister shiftType compareParam regNum cpu =
  pure $ Lens.over CPU.registers (V.// [(fromIntegral regNum, shiftType (fromIntegral vx) 1), (0xF, sb)]) cpu
  where vx = CPU.regVal regNum cpu
        sb = compareParam vx


-- |
-- Sets the register x to (x `op` y)
-- changes the register x
binOpRegisters :: (W.Word8 -> W.Word8 -> W.Word8) -> W.Word8 -> W.Word8 -> Instruction
binOpRegisters op x y cpu =
  setRegister x (CPU.regVal x cpu `op` CPU.regVal y cpu) cpu

-- |
-- stores a number of registers in memory under index
-- changes the memory cells index..(index + @reg)
storeRegInMemory :: W.Word8 -> Instruction
storeRegInMemory reg cpu =
  let
    endReg = fromIntegral reg + 1
    index  = fromIntegral $ Lens.view CPU.index cpu
  in
    pure $
      Lens.over CPU.memory
        (`V.update`
          V.zip (V.enumFromN index (index + endReg))
                (V.slice 0 endReg (Lens.view CPU.registers cpu)))
        cpu

-- |
-- stores a number of memory cells under index in registers
-- changes the registers 0..@reg
storeMemoryInReg :: W.Word8 -> Instruction
storeMemoryInReg reg cpu =
  let
    endReg = fromIntegral reg + 1
    index  = fromIntegral $ Lens.view CPU.index cpu
  in
    pure $
      Lens.over CPU.registers
        (`V.update`
          V.zip (V.enumFromN 0 endReg)
                (V.slice index endReg (Lens.view CPU.memory cpu)))
        cpu


-- |
-- tries to check regarding keypad key
isKey :: (Bool -> Bool) -> W.Word8 -> CPU.CPU -> Emulate Bool
isKey test reg cpu = fmap test keyVal
  where keyVal = Lens.view CPU.keypad cpu V.!? fromIntegral (CPU.regVal reg cpu)
                 `withDefault` throwErr cpu "Bad keypad value in register"

-- |
-- Draws a sprite of size (8 * times) from memory at location (x,y)
-- Changes gfx (and register 0xF if collision detected)
drawSprite :: W.Word8 -> W.Word8 -> W.Word8 -> Instruction
drawSprite x y times cpu =
  let
    index  = Lens.view CPU.index cpu
    (collision, sprite) = arrangePixels (fromIntegral $ CPU.regVal x cpu) (fromIntegral $ CPU.regVal y cpu) (fromIntegral index) (fromIntegral times) cpu
  in
    pure $
      Lens.over CPU.registers (V.// [(0xF, if collision then 1 else 0)]) $
        Lens.over CPU.gfx (`V.update` sprite) cpu

arrangePixels :: Int -> Int -> Int -> Int -> CPU.CPU -> (Bool, V.Vector (Int, Bool))
arrangePixels x y index times cpu =
  unmergePixels $
    V.zipWith3 mergePixels
      (indicesVector x y times)
      (V.backpermute
        (Lens.view CPU.gfx cpu)
        (indicesVector x y times))
      (V.concatMap byteVector $
        V.slice index times (Lens.view CPU.memory cpu))

unmergePixels :: V.Vector (Int, Bool, Bool) -> (Bool, V.Vector (Int, Bool))
unmergePixels vec =
  (V.foldl (\acc (_, o, n) -> acc || (o && not (o `xor` n))) False vec
  ,V.map (\(i, o, n) -> (i, o `xor` n)) vec
  )

mergePixels :: Int -> Bool -> Bool -> (Int, Bool, Bool)
mergePixels = (,,)


indicesVector :: Int -> Int -> Int -> V.Vector Int
indicesVector x y times =

  V.concatMap
    (V.map (`mod` 2048) . separateIndex)
    (V.enumFromStepN (y*64 + x) 64 times)

separateIndex :: Int -> V.Vector Int
separateIndex i =
  V.fromList [i..i+7]

byteVector :: W.Word8 -> V.Vector Bool
byteVector byte = V.fromList (reverse $ go 0x1)
  where go 0x80 = [0x80 == 0x80 .&. byte]
        go n    = (n    == n    .&. byte) : go (n `shiftL` 1)

waitForKey :: W.Word8 -> Instruction
waitForKey reg cpu =
  pure $ case True `V.elemIndex` Lens.view CPU.keypad cpu of
    Just key ->
      nextPC $ Lens.over CPU.registers (V.// [(fromIntegral reg, fromIntegral key)]) cpu
    Nothing ->
      cpu

setRegWithRand :: W.Word8 -> W.Word8 -> Instruction
setRegWithRand nn reg cpu =
  setRegister reg (randNum .&. nn) $ Lens.set CPU.randSeed nextSeed cpu
  where (randNum, nextSeed) = Rand.random $ Lens.view CPU.randSeed cpu

