{- |
The TODO file management tool.
-}
import Control.Monad
import Control.Exception
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
    Just contents -> do
      putStrLn $ unlines $ zipWith (\id item -> printf "%03d : %s" id item) ([0..]::[Int]) contents
    where
      handler :: IOException -> IO ()
      handler e = do hPutStrLn stderr $ "ERROR: " ++ show e ++ " in '" ++ filename ++ "'."

-- | The 'add' command adds the new TODO item to the file.
add :: String   -- ^ The TODO item filename.
    -> [String] -- ^ New TODO items that you want to add.
    -> IO ()    -- ^ Return the unit type.
add filename args = do
  when (length args == 0) $ do
    hPutStrLn stderr $ "WARNING: no todo item. 'add' command needs one or more todo items."
  r <- readTodoFile filename
  case r of
    Nothing -> return ()
    Just contents -> do
      let newItems = unlines $ contents ++ args
      catch
        (do
          (tempName, tempHandle) <- openTempFile "." "todotemp"
          hPutStr tempHandle newItems
          renameFile filename tempName)
        (\e -> do hPutStrLn stderr $ "ERROR " ++ show (e :: IOException) ++ " in tempfile")

-- | Read TODO file.
readTodoFile :: String   -- ^ The TODO item filename.
  -> IO (Maybe [String]) -- ^ If success then return the TODO item list, otherwise Nothing.
readTodoFile filename = do
  r <- try (do readFile filename)
  case r of
    Left e -> do
      hPutStrLn stderr $ "WARNING: " ++ show (e :: IOException) ++ " in '" ++ filename ++ "'."
      return Nothing
    Right contents -> return $ Just $ lines contents

-- | The 'remove' command removes existing TODO item in the file.
remove :: String   -- ^ The TODO item filename.
       -> [String] -- ^ Existing TODO item id list that you want to remove.
       -> IO ()    -- ^ Return the unit type.
remove filename args = do
  if (length args == 0)
  then -- Interactive mode.
    putStrLn "remove command"
  else -- command mode.
    putStrLn "remove command"

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
