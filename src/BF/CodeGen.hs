{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module BF.CodeGen
    (
    ) where

import           Control.Applicative
import           Control.Monad.Trans.Except
import           Control.Monad.State
import qualified Data.Map as Map
import           Data.Maybe

import qualified LLVM.General.Module as M
import           LLVM.General.Context as C
import           LLVM.General.AST
import           LLVM.General.AST.Global as G
import           LLVM.General.AST.Constant
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V

import BF.Syntax

data CGState = CGState { blocks :: Map.Map Name Block
                       , currentBlock :: Name
                       , names :: Map.Map String Int
                       } deriving (Show)

data Block = Block { name :: Name
                   , instructions :: [Named Instruction]
                   , terminator :: Maybe (Named Terminator)
                   } deriving (Show)

newtype CodeGen a = CodeGen (State CGState a)
    deriving (Functor, Applicative, Monad, MonadState CGState)

initialCGState :: CGState
initialCGState = CGState initialBlocks entry Map.empty
    where entry = Name "entry"
          initialBlocks = Map.insert entry defaultBlock{BF.CodeGen.name=entry} Map.empty

runCodeGen :: CodeGen a -> a
runCodeGen (CodeGen cg) = evalState cg initialCGState

--- Name management

newName :: String -> CodeGen Name
newName name = do
    st <- get
    let ns = names st
        count = Map.findWithDefault 0 name ns
    put st{names=Map.insert name (count+1) ns}
    return $ Name (name ++ show count)

--- Block management

defaultBlock :: Block
defaultBlock = Block (error "unnamed Block") [] Nothing

newBlock :: String -> CodeGen Name
newBlock name = do
    st <- get
    name' <- newName name
    let bs = blocks st
        block = defaultBlock
    put st{blocks=Map.insert name' block bs}
    return name'

setBlock :: Name -> CodeGen ()
setBlock n = modify (\s -> s{currentBlock = n})

getBlockName :: CodeGen Name
getBlockName = currentBlock <$> get

-- Partial function!
getBlock :: CodeGen Block
getBlock = do
    blks <- blocks <$> get
    fromJust <$> (flip Map.lookup blks <$> getBlockName)

updateBlock :: Block -> CodeGen ()
updateBlock blk = modify (\s@CGState{blocks} ->
    let blocks' = Map.insert (BF.CodeGen.name blk) blk blocks
    in  s{blocks=blocks'})

-- Partial function!
setTerminator :: Terminator -> CodeGen ()
setTerminator t = do
    blk <- getBlock
    let blk' = blk{terminator = Just $ Do t}
    updateBlock blk'

-- Partial function!
compileBlock :: Block -> BasicBlock
compileBlock (Block {..}) = BasicBlock name (reverse instructions) (fromJust terminator)

--- Types

i8 :: Type
i8 = IntegerType 8

i32 :: Type
i32 = IntegerType 32

--- Functions

makeParameters :: [Type] -> [Parameter]
makeParameters = fmap (\t -> Parameter t (UnName 0) [])

extern :: String -> Type -> [Type] -> Definition
extern name retT params = GlobalDefinition functionDefaults{G.name=Name name, returnType=retT, parameters=(makeParameters params, False)}

function :: String -> Type -> [Type] -> [Block] -> Definition
function name retT params body = GlobalDefinition functionDefaults{G.name=Name name, returnType=retT, parameters=(makeParameters params, False), basicBlocks=map compileBlock body}

--- Terminators

ret :: Maybe Operand -> CodeGen ()
ret op = setTerminator $ Ret op []

--- Instructions

addInstruction :: Named Instruction -> CodeGen ()
addInstruction inst = do
    block <- getBlock
    let insts  = instructions block
        block' = block{instructions=inst:insts}
    updateBlock block'

call :: Maybe String -> Operand -> [Operand] -> CodeGen (Maybe Name)
call mname func args = do
    let inst = Call False CC.C [] (Right func) (makeArguments args) [] []
    (name, instr) <- case mname of
                          Nothing -> return (Nothing, Do inst)
                          Just a -> newName a >>= (\name -> return (Just name, name := inst))
    addInstruction instr
    return name
    where makeArguments = fmap (\a -> (a, []))

--- Definitions

putchar :: Definition
putchar = extern "putchar" i32 [i32]

getchar :: Definition
getchar = extern "getchar" i32 []

testModule :: Module
testModule = defaultModule{moduleName="test", moduleDefinitions=[putchar, getchar]}

-------

cint :: Type
cint = IntegerType 8

--testModule :: Module
--testModule = defaultModule{moduleName="test", moduleDefinitions=[testDef, testDef2]}

testDef :: Definition
testDef = GlobalDefinition testFunc

testFunc :: Global
testFunc = functionDefaults{G.name=Name "putchar", returnType=IntegerType 32, parameters=([testParam], False)}

testParam :: Parameter
testParam = Parameter (IntegerType 32) (Name "1") []

testFuncType :: Type
testFuncType = FunctionType (IntegerType 32) [IntegerType 32] False

testDef2 :: Definition
testDef2 = GlobalDefinition testFunc2

testFunc2 :: Global
testFunc2 = Function L.External V.Default CC.C [] VoidType (Name "main") ([], False) [] Nothing 0 Nothing testFunc2Blocks

testFunc2Blocks :: [BasicBlock]
testFunc2Blocks = [ BasicBlock (Name "asdf") insts2 (Do $ Br (Name "asdf") [])
                  ]

insts2 :: [Named Instruction]
insts2 = [ Do $ Call False CC.C [] (Right $ ConstantOperand $ GlobalReference testFuncType (Name "putchar")) [(ConstantOperand $ Int 32 84, [])] [] []
         ]

---------

unExcept :: ExceptT String IO a -> IO a
unExcept a = do
    asd <- runExceptT a
    either (const undefined) return asd

run :: Context -> (M.Module -> IO a) -> IO a
run c f = unExcept $ M.withModuleFromAST c testModule f

run2 :: (M.Module -> IO a) -> IO a
run2 f = withContext (flip run f)
