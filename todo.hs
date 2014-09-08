{- |
The TODO file management tool.
-}
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.List
import Data.Maybe
import Text.Read
import System.Directory
import System.Environment
import System.IO
import System.IO.Error
import Text.Printf

main :: IO()
main = do
  args <- getArgs
  dispatch args

-- | Dispatch the command line arguments, and run the command passed.
dispatch :: [String] -- ^ The command line arguments.
       -> IO()    -- ^ Return the unit type.
dispatch (filename:"view":args) = view filename args
dispatch (filename:"add":args) = add filename args
dispatch (filename:"remove":args) = remove filename args
dispatch (filename:"rm":args) = remove filename args
dispatch (filename:"bump":args) = bump filename args
dispatch (_:cmd:_) = do putStrLn $ "\"" ++ cmd ++ "\" is unknown command."
dispatch (filename:[]) = view filename []
dispatch _ = showUsage

-- | The 'view' command lists the TODO item in the file.
view :: String   -- ^ The TODO item filename.
     -> [String] -- ^ Should be empty list. If this is no empty then display the warning message.
     -> IO ()    -- ^ Return the unit type.
view filename args = do
  when (length args > 0) $ do
    putStrLn $ "WARNING: unnecessary arguments detected: " ++ (foldr1 (\l r -> l ++ " " ++ r) args)
  r <- readTodoFile filename
  case r of
    Nothing -> return ()
    Just contents -> viewItemList contents
    where
      handler :: IOException -> IO ()
      handler e = do hPutStrLn stderr $ "ERROR: " ++ show e ++ " in '" ++ filename ++ "'."

viewItemList :: [String] -> IO ()
viewItemList contents = putStrLn $ unlines $ zipWith (\id item -> printf "%03d : %s" id item) ([0..]::[Int]) contents

-- | The 'add' command adds the new TODO item to the file.
add :: String   -- ^ The TODO item filename.
    -> [String] -- ^ New TODO items that you want to add. If empty, display the error message.
    -> IO ()    -- ^ Return the unit type.
add filename args =
  if (length args == 0) then do
    hPutStrLn stderr $ "ERROR: no todo item. 'add' command needs one or more todo items."
  else do
    r <- readTodoFile filename
    case r of
      Nothing -> return ()
      Just contents -> do
        writeTodoFile filename . unlines $ contents ++ args

-- | Read TODO file.
readTodoFile :: String   -- ^ The TODO item filename.
  -> IO (Maybe [String]) -- ^ If success then return the TODO item list, otherwise Nothing.
readTodoFile filename = do
  r <- try (do readFile filename)
  case r of
    Left e -> do
      hPutStrLn stderr $ "ERROR: " ++ show (e :: IOException) ++ " in '" ++ filename ++ "'."
      return Nothing
    Right contents -> return $ Just $ lines contents

-- | Write TODO file.
writeTodoFile :: String -- ^ The TODO item filename.
              -> String -- ^ The contents that to be writte.
              -> IO ()
writeTodoFile filename contents =
  catch
    (bracketOnError
      (openTempFile "." "todotemp")
      (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
      (\(tempName, tempHandle) -> do
        hPutStr tempHandle contents
        hClose tempHandle
        renameFile tempName filename))
    (\e -> do hPutStrLn stderr $ "ERROR: " ++ show (e :: IOException) ++ " in tempfile")

-- | The 'remove' command removes existing TODO item in the file.
remove :: String   -- ^ The TODO item filename.
       -> [String] -- ^ Existing TODO item id list that you want to remove.
       -> IO ()    -- ^ Return the unit type.
remove filename args = do
  if (length args == 0)
  then (do -- Interactive mode.
    view filename []
    putStrLn "Enter the item id you want to remove:"
    id <- getLine
    removeItems [id])
  else removeItems args -- command mode.
  where
    removeItems :: [String] -> IO ()
    removeItems args = do
      r <- readTodoFile filename
      case r of
        Nothing -> return ()
        Just contents -> do
          let parsedArgs = map (readMaybe :: String -> Maybe Int) args
          if Nothing `elem` parsedArgs then (do
            hPutStrLn stderr "ERROR: Couldn't parse argments:"
            mapM_ (\e -> hPutStr stderr $ " " ++ e) args)
          else do
            let idList = rm contents . extract contents . nub $ map fromJust parsedArgs
            writeTodoFile filename $ unlines idList
            where
                extract :: [String] -> [Int] -> [String]
                extract lst idList = foldr (\n v -> if (n >= 0 && n < (length lst)) then ((lst !! n) : v) else v) [] idList
                rm :: [String] -> [String] -> [String]
                rm lst rmList = foldr (\n v -> if (n `elem` rmList) then v else n:v) [] lst

-- | Bump up TODO item position.
bump :: String   -- ^ The TODO item filename.
     -> [String] -- ^ Existing TODO item id list that you want to bump.
     -> IO ()
bump filename args = do
  r <- readTodoFile filename
  case r of
    Nothing -> return ()
    Just contents -> do
      if (length args == 0) then do
        newItems <- interactiveBump contents
        when (notEqForList contents newItems) $ writeTodoFile filename $ unlines newItems
        return ()
      else do
        newItems <- bumpItem (head args) contents
        when (notEqForList contents newItems) $ writeTodoFile filename $ unlines newItems
        return ()
  where
    bumpItem :: String -> [String] -> IO ([String])
    bumpItem id contents =
      let offset = (read :: String -> Int) id
      in if ((offset > 0) && (offset < length contents)) then
        let front = take (offset - 1) contents
            prev = [contents !! (offset - 1)]
            target = [contents !! offset]
            back = drop (offset + 1) contents
        in return $ front ++ target ++ prev ++ back
      else do
        hPutStrLn stderr $ "ERROR: '" ++ id ++ "' is out of range."
        return contents
    interactiveBump :: [String] -> IO ([String])
    interactiveBump contents = do
      viewItemList contents
      putStrLn "Enter the item id you want to bump up:"
      id <- getLine
      if (id == "q" || id == "Q") then
        return contents
      else do
        y <- (bumpItem id contents) >>= interactiveBump
        return y
    notEqForList :: (Eq a) => [a] -> [a] -> Bool
    notEqForList lhs rhs = foldr (\(l, r) v -> v || not (l == r)) False $ zip lhs rhs

-- | Display the usage of this program.
showUsage :: IO() -- ^ Return the unit type.
showUsage = do
  progName <- getProgName
  mapM_ putStrLn [
    progName,
    "  The todo file management tool.",
    "",
    "  usage: " ++ progName ++ " filename [command] [argments]",
    "",
    "  add todo-item      : add todo-item.",
    "  remove todo-item-id: remove todo-item.",
    "  rm todo-item-id    : this is same \"remove\" command.",
    "  view               : show todo-item list.",
    "  (no command)       : show this messages." ]
