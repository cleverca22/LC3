{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Lens
import           Control.Monad.State
import           Data.Bits
import           Data.Bits.Lens
import           Data.Bool
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.List
import           Data.Proxy
import qualified Data.Text as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Word
import           GHC.TypeLits
import           System.Environment
import           System.Exit
import           System.IO
import           Test.Hspec
import           Formatting
import           Data.Binary.Get

newtype Memory (size :: Nat)
  = Memory { _mem :: Vector Word16 }
  deriving (Show, Eq)

newtype Registers (size :: Nat)
  = Registers { _reg :: Vector Word16 }
 deriving (Show, Eq)

data R
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | PC
  | Cond
  | Count
  deriving (Eq, Show, Enum)

reg' :: R -> Lens' (Registers (nat :: Nat)) Word16
reg' n = lens (\(Registers v) -> v V.! fromEnum n) setter
  where
    setter (Registers vec) word16 =
      Registers $ vec V.// [(fromEnum n, word16)]

mem' :: Int -> Lens' (Memory (nat :: Nat)) Word16
mem' n = lens (\(Memory v) -> v V.! n) setter
  where
    setter (Memory vec) word16 =
      Memory $ vec V.// [(n, word16)]

data Machine
  = Machine
  { _machineReg :: Registers 11
  , _machineMem :: Memory 65536
  , _machineStatus :: Status
  }

status :: Lens' Machine Status
status =
  lens _machineStatus $ \p x ->
    p { _machineStatus = x }

data Status
  = Running
  | Halt
  deriving (Show, Eq)

reg :: R -> Lens' Machine Word16
reg r = machineReg . reg' r

mem :: Int -> Lens' Machine Word16
mem n = machineMem . mem' n

machineReg :: Lens' Machine (Registers 11)
machineReg =
  lens _machineReg (\m r -> m { _machineReg = r })

machineMem :: Lens' Machine (Memory 65536)
machineMem =
  lens _machineMem (\m r -> m { _machineMem = r })

registers :: forall n . n ~ 11 => Registers n
registers = Registers (V.replicate n 0x0)
  where
    n = fromIntegral $ natVal (Proxy @ n)

memory :: forall n . n ~ 65536 => Memory n
memory = Memory (V.replicate n 0x0)
  where
    n :: Int
    n = fromIntegral $ natVal (Proxy @ n)

data OpCode
  = BR  -- /* branch */
  | ADD -- /* add  */
  | LD  -- /* load */
  | ST  -- /* store */
  | JSR -- /* jump register */
  | AND -- /* bitwise and */
  | LDR -- /* load register */
  | STR -- /* store register */
  | RTI -- /* unused */
  | NOT -- /* bitwise not */
  | LDI -- /* load indirect */
  | STI -- /* store indirect */
  | JMP -- /* jump */
  | RES -- /* reserved (unused) */
  | LEA -- /* load effective address */
  | TRAP -- /* execute trap */
  deriving (Eq, Ord, Show, Enum)

data OpCode2
  = Lea { opReg :: R, offset :: Word16}
  | Trap { trapCmd :: Word16 }
  | Ldr { ldrR0 :: R, ldrR1 :: R, ldrOffset :: Word16 }
  | AddImm { addImmR0 :: R, addImmR1 :: R, addImm :: Word16 }
  | Add { addR0 :: R, addR1 :: R, addR2 :: R }
  | Ldi { ldiR0 :: R, ldiOffset :: Word16 }
  | Other
  deriving (Show)

type Addr = Word16
type Val  = Word16

memWrite :: Addr -> Val -> Routine ()
memWrite addr val = mem (fromIntegral addr) .= val

getKey :: IO Char
getKey = getChar

checkKey :: IO (Maybe Word16)
checkKey = do
  result <- B.hGetNonBlocking stdin 1
  case result of
    x | B.null x -> pure Nothing
      | otherwise -> do
          let [l] = B.unpack x
          print x
          pure $ Just $ fromIntegral l
        where
          go (l,r) x n
            | n < 8 = setBit x $ popCount (testBit r n)
            | otherwise = setBit x $ popCount (testBit l n)

memRead :: Addr -> Routine Val
memRead (fromIntegral -> addr)
  | addr == mrKBSR = handleKey
  | otherwise = use $ mem addr
    where
      handleKey = do
        maybeKey <- liftIO checkKey
        case maybeKey of
          Just key -> do
            liftIO $ print maybeKey
            mem mrKBSR .= 1 `shiftL` 15
            mem mrKBDR .= key
          Nothing ->
            mem mrKBSR .= 0x0
        use (mem addr)

mrKBSR = 0xFE00 -- /* keyboard status */
mrKBDR = 0xFE02 -- /* keyboard data */

pos, zro, neg :: Word16
pos = 1
zro = 2
neg = 4

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  heap <- readImageFile
  let machine = Machine registers heap Running
  finished <- runRoutine machine routine
  print (finished ^. status)

readImageFile :: IO (Memory 65536)
readImageFile = do
  args <- getArgs
  case args of
    fileName : _ -> do
      rawbs <- LBS.readFile fileName
      let
        parser :: Get (Word16, [Word16])
        parser = do
          origin <- getWord16be
          let
            go :: [Word16] -> Get (Word16,[Word16])
            go result = do
              empty <- isEmpty
              if empty then
                pure (origin, result)
              else do
                word <- getWord16be
                go (result <> [word])
          go []
      let (origin, words) = runGet parser rawbs
      let pad = V.replicate (fromIntegral origin) (0x0 :: Word16)
          mid = V.fromList words
          end = V.replicate (65536 - (V.length pad + V.length mid)) (0x0 :: Word16)
      -- mapM_ print (fmap getOp mid)
      pure $ Memory (pad <> mid <> end)
    _ -> do
      putStrLn "Please enter path to LC3 program"
      exitFailure

test :: IO ()
test = hspec tests

type Routine = StateT Machine IO

signExtend :: Word16 -> Int -> Word16
signExtend x bitCount
  -- shiftL or shiftR? that is the question...
  | x `shiftR` (bitCount - 1) .&. 1 == 1 = x .|. (0xFFFF `shiftL` bitCount)
  | otherwise = x

updateFlags :: R -> Routine ()
updateFlags r = do
  x <- use (reg r)
  case x of
    z | z == 0 -> reg Cond .= zro
      | z ^. bitAt 15 -> reg Cond .= neg
      | otherwise -> reg Cond .= pos

swap16 :: Word16 -> Word16
swap16 x = x `shiftL` 8 .|. x `shiftR` 8

toE :: Enum e => Word16 -> e
toE = toEnum . fromIntegral

getOp :: Word16 -> OpCode
getOp x = toE (x `shiftR` 12)

io :: MonadIO m => IO a -> m a
io = liftIO

routine :: Routine ()
routine = do
  reg PC .= 0x3000
  fix $ \go -> do
    s <- use status
    unless (s == Halt) $ do
      loop >> go

parseInstr :: Word16 -> OpCode2
parseInstr instr = do
  let immMode = instr ^. bitAt 5
  case getOp instr of
    ADD -> do
      let
        r0 = toE $ (instr `shiftR` 9) .&. 0x7
        r1 = toE $ (instr `shiftR` 6) .&. 0x7
      if immMode then do
        let
          imm5 = signExtend (instr .&. 0x1F) 5
        AddImm r0 r1 imm5
      else do
        let
          r2 = toE (instr .&. 0x7)
        Add r0 r1 r2
    LEA -> do
      let
        r0 :: R
        r0 = toE $ (instr `shiftR` 9) .&. 0x7
        offset = signExtend (instr .&. 0x1ff) 9
      Lea r0 offset
    TRAP -> Trap (instr .&. 0xFF)
    LDR -> do
      let
        r0 = toE $ (instr `shiftR` 9) .&. 0x7
        r1 = toE $ (instr `shiftR` 6) .&. 0x7
        offset = signExtend (instr .&. 0x3F) 6
      Ldr r0 r1 offset
    LDI -> do
      let
        dr = toE $ (instr `shiftR` 9) .&. 0x7
        pcOffset = signExtend (instr .&. 0x1ff) 9
      Ldi dr pcOffset
    _ -> Other

dumpRegs :: Routine ()
dumpRegs = do
  pc <- use $ reg PC
  instr <- memRead pc
  r0 <- use $ reg R0
  r1 <- use $ reg R1
  r6 <- use $ reg R6
  let
    immMode = instr ^. bitAt 5
    parsed = parseInstr instr
    op = getOp instr
    fmt :: Format T.Text (Word16 -> Word16 -> Bool -> OpCode -> Word16 -> Word16 -> Word16 -> OpCode2 -> T.Text)
    fmt = "PC: 0x" % hex % " Instr: 0x" % hex % " Imm: " % shown % " OP: " % shown % " R0: 0x" % hex % " R1: 0x" % hex % " R6: 0x" % hex % " OP: " % shown
  liftIO $ do
    hPutStrLn stderr $ T.unpack $ sformat fmt pc instr immMode op r0 r1 r6 parsed
    hFlush stderr

loop :: Routine ()
loop = do
  dumpRegs
  instr <- memRead =<< use (reg PC)
  reg PC += 1
  let immMode = instr ^. bitAt 5
  case getOp instr of
    ADD -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
      if immMode
        then do
          let imm5 = signExtend (instr .&. 0x1F) 5
          result <- (imm5+) <$> use (reg r1)
          reg r0 .= result
        else do
          let r2 = toE (instr .&. 0x7)
          r1' <- use (reg r1)
          r2' <- use (reg r2)
          reg r0 .= r1' + r2'
      updateFlags r0
    LDI -> do
      let dr = toE $ (instr `shiftR` 9) .&. 0x7
          pcOffset = signExtend (instr .&. 0x1ff) 9
      -- reg[r0] = mem_read(mem_read(reg[R_PC] + pc_offset))
      pcVal <- use (reg PC)
      addr <- use $ mem (fromIntegral (pcVal + pcOffset))
      r <- memRead addr
      reg dr .= r
      updateFlags dr
    RTI ->
      pure ()
    RES ->
      pure ()
    AND -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
      if immMode
        then do
          let imm5 = signExtend (instr .&. 0x1F) 5
          r1' <- use (reg r1)
          reg r0 .= r1' .&. imm5
        else do
          let r2 = toE (instr .&. 0x7)
          r1' <- use (reg r1)
          r2' <- use (reg r2)
          reg r0 .= r1' .&. r2'
      updateFlags r0
    NOT -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
      r1' <- use (reg r1)
      reg r0 .= complement r1'
    BR -> do
      let condFlag = (instr `shiftR` 9) .&. 0x7
          pcOffset = signExtend (instr .&. 0x1ff) 9
      rCond <- use (reg Cond)
      when (condFlag .&. rCond /= 0) $
        reg PC += pcOffset
    JMP -> do
      let r1 = toE $ (instr `shiftR` 6) .&. 0x7
      r1' <- use (reg r1)
      reg PC .= r1'
    JSR -> do
      let r1 = toE $ (instr `shiftR` 6) .&. 0x7
          longPCOffset = signExtend (instr .&. 0x7ff) 11
          longFlag = (instr `shiftR` 11) .&. 1
      pc <- use (reg PC)
      reg R7 .= pc
      if longFlag == 1
        then reg PC += longPCOffset
        else reg PC .= r1
    LD -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          pcOffset = signExtend (instr .&. 0x1ff) 9
      pc <- use (reg PC)
      r <- memRead (pc + pcOffset)
      reg r0 .= r
      updateFlags r0
    LDR -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
          offset = signExtend (instr .&. 0x3F) 6
      r1' <- use (reg r1)
      val <- memRead (r1' + offset)
      reg r0 .= val
      updateFlags r0
    LEA -> do
      let
          r0 :: R
          r0 = toE $ (instr `shiftR` 9) .&. 0x7
          offset = signExtend (instr .&. 0x1ff) 9
      pc <- use (reg PC)
      let val = (pc + offset)
      reg r0 .= val
    ST -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          offset = signExtend (instr .&. 0x1ff) 9
      pc <- use (reg PC)
      memWrite (pc + offset) r0
    STI -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          offset = signExtend (instr .&. 0x1ff) 9
      r0' <- use (reg r0)
      pc <- use (reg PC)
      val <- memRead (pc + offset)
      memWrite val r0'
    STR -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
          offset = signExtend (instr .&. 0x3F) 6
      r0' <- use (reg r0)
      r1' <- use (reg r1)
      memWrite (r1' + offset) r0'
    TRAP -> do
      case instr .&. 0xFF of
        t | trapGetc == t -> do
              r <- fromIntegral . ord <$> liftIO getChar
              liftIO $ hPutStrLn stderr $ "trapgetc read: " <> (show r)
              reg R0 .= r
          | trapPuts == t -> do
              v <- use (reg R0)
              let
                loop 100 = liftIO exitFailure
                loop x = do
                    val <- memRead x
                    liftIO $ hPutStrLn stderr $ "reading " <> (show x) <> " " <> (show val)
                    if val == 0x0000 then
                      pure ()
                    else do
                      let c = chr (fromIntegral val)
                      liftIO (putChar c)
                      loop (x+1)
              loop v
              liftIO $ hFlush stdout
          | trapPutsp == t -> do
              v <- use (reg R0)
              let loop x = do
                    val <- memRead x
                    unless (val == 0x0000) $ do
                      let char1 = chr (fromIntegral (val .&. 0xFF))
                          char2 = chr (fromIntegral (val `shiftR` 8))
                      liftIO $ mapM_ putChar [char1, char2]
                    loop (x+1)
              loop v
          | trapOut == t -> do
              liftIO . putChar =<<
                chr . fromIntegral <$> use (reg R0)
          | trapIn == t -> do
              r <- fromIntegral . ord <$> liftIO getChar
              reg R0 .= r
          | trapHalt == t -> do
              liftIO (putStrLn "HALT")
              status .= Halt
          | otherwise -> do
              liftIO $ do
                print (getOp instr)
                print instr
                exitFailure

pcStart :: Int
pcStart = fromIntegral 0x3000

runRoutine :: Machine -> Routine () -> IO Machine
runRoutine = flip execStateT

-- in the trap

trapGetc :: Word16
trapGetc = 0x20  --  /* get character from keyboard */

trapOut :: Word16
trapOut = 0x21   --  /* output a character */

trapPuts :: Word16
trapPuts = 0x22  --  /* output a word string */

trapIn :: Word16
trapIn = 0x23    --  /* input a string */

trapPutsp :: Word16
trapPutsp = 0x24 --  /* output a byte string */

trapHalt :: Word16
trapHalt = 0x25  --  /* halt the program */

-- | some tests

tests :: Spec
tests = do
  describe "VM tests" $ do
    addTwoNumbers
    addTwoNumbersImm
    andTwoNumbers
    andTwoNumbersImm
    complementNumber

complementNumber :: SpecWith ()
complementNumber =
  it "Should NOT (complement) a number" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` (-2)
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b1001101011000100
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 1

andTwoNumbers :: SpecWith ()
andTwoNumbers =
  it "Should AND two numbers" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` 0
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b0101101011000100
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 5
           & reg' R4 .~ 2

andTwoNumbersImm :: SpecWith ()
andTwoNumbersImm =
  it "Should AND two numbers w/ immediate" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` 1
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b0101101011111111
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 1

addTwoNumbers :: SpecWith ()
addTwoNumbers =
  it "Should ADD two numbers" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` 2
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b0001101011000100
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 1
           & reg' R4 .~ 1

addTwoNumbersImm :: SpecWith ()
addTwoNumbersImm =
  it "Should ADD two numbers w/ immediate" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` 32
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b0001101011111111
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 1

haltInstr = 0b1111000000100101

-- k' = signExtend (0b0001101011111111 .&. 0x1F) 5
