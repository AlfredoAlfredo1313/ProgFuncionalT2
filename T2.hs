import System.IO  
import Text.Regex.Posix

type Memory = [(Int, Int)]

data Computer   =  NextStep {getMem::Memory, getRem::Int, getRdm::Int, getAcc::Int, getPc::Int, getEqz::Bool} 
                 | Fim Memory | Crash0 String Memory | Crash1 String Computer deriving(Show)

  
main = do  
    let funFilter = (\(x:xs) -> not $ x == '#')
    let parse2mem = (\n -> (\(a:b:_) -> (read a :: Int, read b :: Int)) (getAllTextMatches $ n =~ "-?([0-9]|[1-9][0-9]*)" :: [String]))
    print "Reading Programa1"
    handle <- openFile "Programa1.txt" ReadMode  
    contents <- hGetContents handle  
    let list = lines contents
    let mem = map parse2mem $ filter funFilter list
    let comp = compileProgram mem (buildMemory 255)
    print "Results from Programa1"
    print comp
    print "Reading Programa2"
    handle <- openFile "Programa2.txt" ReadMode  
    contents <- hGetContents handle  
    let list = lines contents
    let mem = map parse2mem $ filter funFilter list
    let comp = compileProgram mem (buildMemory 255)
    print "Results from Programa2"
    print comp
    print "Reading Programa3"
    handle <- openFile "Programa3.txt" ReadMode  
    contents <- hGetContents handle  
    let list = lines contents
    let mem = map parse2mem $ filter funFilter list
    let comp = compileProgram mem (buildMemory 255)
    print "Results from Programa3"
    print comp
    hClose handle  

buildMemory :: Int -> Memory
buildMemory 0 = [(0, 0)]
buildMemory n = buildMemory (n - 1) ++ [(n, 0)]

compileProgram :: Memory -> Memory -> Either Computer Computer
compileProgram [] mem = getNextStep (NextStep mem 0 0 0 0 False)
compileProgram ((line,ins):xs) mem = evalInstruction (line, ins) >>= \n -> compileProgram xs newMem
    where
        (z,_:zs) = splitAt line mem
        newMem = z ++ (line, ins) : zs

evalInstruction :: (Int, Int) -> Either Computer Computer
evalInstruction (line, ins) = case (ins <= 256 && ins >= 0) of
    True  -> Right (Fim [])
    False -> Left (Crash0 "Compilation Error: Value Greater Than 8 bits in" [(line, ins)])


getNextStep :: Computer -> Either Computer Computer
getNextStep (Crash0 s mem) = Left (Crash0 s mem) 
getNextStep (Fim mem) = Right (Fim mem)
getNextStep (Crash1 s c) = Left (Crash1 s c) 
getNextStep c = evalComputer c >>= \n -> getNextStep (instruction n arg)
    where
        (_, instructionCode) = (getMem c)!!getPc c
        instruction = readInstruction instructionCode
        (_, arg) = (getMem c)!!(getPc c + 1)

evalComputer :: Computer -> Either Computer Computer
evalComputer c = case ((getPc c) < 0 || mod (getPc c) 2 == 1) of
    True  -> Left (Crash1 "Runtime Error. Invalid Program Counter" c)
    False -> Right (c)

readInstruction :: Int -> (Computer -> Int -> Computer)
readInstruction code = case code of
                            2  -> execLOD
                            4  -> execSTO
                            6  -> execJMP
                            8  -> execJMZ
                            10 -> execCPE
                            14 -> execADD
                            16 -> execSUB
                            18 -> execNOP
                            20 -> execHLT
                            _  -> unkExec


execLOD :: Computer -> Int -> Computer
execLOD c arg = (NextStep (getMem c) (getRem c) (getRdm c) newACC ((getPc c)+2) (newACC == 0))
    where 
        (_, newACC) = (getMem c)!!arg

execSTO :: Computer -> Int -> Computer
execSTO c arg = (NextStep newMem (getRem c) (getRdm c) (getAcc c) ((getPc c)+2) (getEqz c))
    where 
        (x,_:xs) = splitAt arg (getMem c)
        newMem = x ++ (arg, getAcc c) : xs

execJMP :: Computer -> Int -> Computer
execJMP c arg = (NextStep (getMem c) (getRem c) (getRdm c) (getAcc c) newPC (getEqz c))
    where 
        (_, newPC) = (getMem c)!!arg

execJMZ :: Computer -> Int -> Computer
execJMZ c arg = (NextStep (getMem c) (getRem c) (getRdm c) (getAcc c) newPC (getEqz c))
    where 
        (_, rVal) = (getMem c)!!arg
        newPC = case (getAcc c) of
                    0 -> rVal
                    _ -> getPc c + 2

execCPE :: Computer -> Int -> Computer
execCPE c arg = (NextStep (getMem c) (getRem c) (getRdm c) newACC ((getPc c)+2) (newACC == 0))
    where 
        (_, val) = (getMem c)!!arg
        newACC = case (val == (getAcc c)) of
                    True  -> 0
                    False -> 1

execADD :: Computer -> Int -> Computer
execADD c arg = (NextStep (getMem c) (getRem c) (getRdm c) newACC ((getPc c)+2) (newACC == 0))
    where 
        (_, val) = (getMem c)!!arg
        aux      = (getAcc c) + val
        newACC   = mod ((getAcc c) + val) 256

execSUB :: Computer -> Int -> Computer
execSUB c arg = (NextStep (getMem c) (getRem c) (getRdm c) newACC ((getPc c)+2) (newACC == 0))
    where 
        (_, val) = (getMem c)!!arg
        aux      = (getAcc c) - val
        newACC   = case (aux < 0) of
                    True  -> (256 + aux)
                    False -> aux

execNOP :: Computer -> Int -> Computer
execNOP c arg = (NextStep (getMem c) (getRem c) (getRdm c) (getAcc c) ((getPc c)+2) (getEqz c))

execHLT :: Computer -> Int -> Computer
execHLT c arg = Fim (getMem c)

unkExec :: Computer -> Int -> Computer
unkExec c arg = (Crash1 "Runtime Error. Invalid Instruction in" c)
