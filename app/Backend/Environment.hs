
module Backend.Environment where

import Frontend.Syntax
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Data.IORef
import Control.Monad

type Environment = MV.IOVector Frame

type Frame = MV.IOVector Binding

type Binding = IORef (String, Expression)

firstFrame :: Environment -> IO Frame
firstFrame env = MV.read env (MV.length env - 1)

restFrames :: Environment -> Environment
restFrames env = MV.take (MV.length env - 1) env 

noMoreFrames :: Environment -> Bool
noMoreFrames = MV.null

adjoinFrame :: Frame -> Environment -> IO Environment
adjoinFrame frame env = do
    env' <- MV.grow env 1
    MV.write env' (MV.length env' - 1) frame
    return env'

adjoinBinding :: Binding -> Frame -> IO Frame
adjoinBinding binding frame = do
    frame' <- MV.grow frame 1
    MV.write frame' (MV.length frame' - 1) binding
    return frame'

setFirstFrame :: Environment -> Frame -> IO ()
setFirstFrame env = MV.write env (MV.length env - 1)

setBindingValue :: Binding -> Expression -> IO ()
setBindingValue binding val = do
    (var, _) <- readIORef binding
    writeIORef binding (var, val)

bindingVariable :: Binding -> IO String
bindingVariable binding = do
    (var, _) <- readIORef binding
    return var

bindingValue :: Binding -> IO Expression
bindingValue binding = do
    (_, val) <- readIORef binding
    return val

makeBinding :: String -> Expression -> IO Binding
makeBinding var val = newIORef (var, val)

makeFrames :: [String] -> [Expression] -> IO Frame
makeFrames vars vals = do
    frame <- MV.new (length vars)
    mapM_ (go frame) (zip [0..] (zip vars vals))
    return frame
    where
        go frame (index, (var, val)) = do
            binding <- makeBinding var val
            MV.write frame index binding

findBindingInFrame :: String -> Frame -> IO (Maybe Binding)
findBindingInFrame var frame = do
    bindings' <- V.freeze frame
    let bindings = V.toList bindings'
    foldM go Nothing bindings
    where
        go Nothing binding = do
            var' <- bindingVariable binding
            if var == var'
                then return $ Just binding
                else return Nothing
        go (Just binding) _ = return $ Just binding

findBindingInEnv :: String -> Environment -> IO (Maybe Binding)
findBindingInEnv var env = do
    frames' <- V.freeze env
    let frames = reverse (V.toList frames')
    foldM go Nothing frames
    where
        go Nothing frame = findBindingInFrame var frame
        go (Just binding) _ = return $ Just binding

lookUpVariable :: String -> Environment -> IO Expression
lookUpVariable var env = do
    binding <- findBindingInEnv var env
    case binding of
        Nothing -> return $ ExprVar var 
        Just binding' -> bindingValue binding'

extendEnvironment :: [String] -> [Expression] -> Environment -> IO Environment
extendEnvironment vars vals env = do
    frame <- makeFrames vars vals
    adjoinFrame frame env

setVariableValue :: String -> Expression -> Environment -> IO ()
setVariableValue var val env = do
    binding <- findBindingInEnv var env
    case binding of
        Nothing -> error $ "Variable " ++ var ++ " not found"
        Just binding' -> setBindingValue binding' val

defineVariable :: String -> Expression -> Environment -> IO ()
defineVariable var val env = do
    frame <- firstFrame env
    binding <- findBindingInFrame var frame
    case binding of
        Nothing -> do
            binding' <- makeBinding var val
            frame' <- adjoinBinding binding' frame
            setFirstFrame env frame'
        Just binding' -> setBindingValue binding' val

setupEnvironment :: IO Environment
setupEnvironment = do
    frame <- makeFrames ["True", "False"] [ExprInt 1, ExprInt 0]
    env <- MV.new 1
    MV.write env 0 frame
    return env

prettyPrint :: Environment -> IO ()
prettyPrint env = do
    frames' <- V.freeze env
    let frames = V.toList frames'
    mapM_ go frames
    where
        go frame = do
            putStrLn "-----------------------------------------------------"
            bindings' <- V.freeze frame
            let bindings = V.toList bindings'
            mapM_ go' bindings
            putStrLn "-----------------------------------------------------"
        go' binding = do
            (var, val) <- readIORef binding
            putStrLn $ "|  " ++ var ++ " = " ++ show val