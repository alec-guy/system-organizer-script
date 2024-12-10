module Main where

import Data.Text as T hiding (find)
-- Objective One 
-- Finds all files in a directory and its subdirectories. 
-- Objective Two 
-- Sort these files into folders based on their extension

import Turtle.Shell (sh, Shell(..), select, view, reduce)
import Turtle.Prelude (ls, find, testdir, testfile, lstree, mkdir, cp)
import Turtle (splitDirectories, liftIO, Fold(..), hasExtension, extension)
import Control.Monad (filterM, mapM, sequence, liftM2, sequence_)
import Data.Map.Strict (fromList, Map(..), (!), size, singleton, elems)
import Data.List (nub, partition, delete)
import Data.Maybe (fromJust)
import Data.Char (toUpper, toLower)
import Data.Set as Set (fromList, union, null, empty, map, elemAt , deleteAt, toList, Set(..), partition, singleton, size)
import Control.Exception (try, SomeException)
-- sh :: MonadIO io => Shell a -> io ()

main :: IO ()
main = sh $ do 
  let workingDirectory = ls "./"  
  paths                                  <- getPaths workingDirectory
  parentSubdirectories                   <- liftIO $ getSubdirectories paths 
  parentFiles                            <- liftIO $ getFiles paths
  let treeDict                           = makeTrees parentSubdirectories
  treePaths                              <- (sequence $  lookupTreeVals parentSubdirectories treeDict) :: Shell [FilePath]
  childrenSubs                           <- liftIO $ getSubdirectories treePaths 
  childrenFiles                          <- liftIO $ getFiles treePaths 
  mapSubsFiles                           <- (main2 childrenSubs) :: Shell [Map [Subdirectory] [File]]
  let files = (Set.fromList $ getOnlyFiles mapSubsFiles) `union` (Set.fromList parentFiles) `union` (Set.fromList childrenFiles)
            where getOnlyFiles mapSF = Prelude.concat ((Prelude.concat (elems <$> mapSF)) :: [[File]])     
      counter = 0
      folders = sortFilesByExtension counter files 
  liftIO $ makeFolders folders 

makeFolders :: Set Folder -> IO ()
makeFolders folders = sequence_ (makeFolder <$> (Set.toList folders))
     where makeFolder :: Folder -> IO () 
           makeFolder folder = do 
             result <- Control.Exception.try (mkdir (folderName folder))  :: (IO (Either SomeException ()))
             case result of 
                Left _  -> pure ()
                Right _ -> pure ()
         


sortFilesByExtension :: Int -> Set File -> Set Folder
sortFilesByExtension counter files = 
    if counter >= (Set.size files) 
        then Set.empty 
        else case Set.null files of 
              True      -> Set.empty
              False     -> case (Set.partition (hasSameExtension (elemAt counter files)) (deleteAt counter files)) of 
                              (setThatDoes, setThatDoesn't) -> 
                                       Set.singleton 
                                       (
                                       ( Folder 
                                         { folderName  = "./" ++ (mkFolderName (extension' (elemAt counter files))) ++ "alec"
                                         , folderFiles = setThatDoes
                                         }
                                       ) 
                                       )`union` (sortFilesByExtension (counter + 1) setThatDoesn't)

hasSameExtension :: File -> File -> Bool 
hasSameExtension file1 file2 = (extension' file1) == (extension' file2)

extension' :: File -> String 
extension' (File filepath) = 
    case hasExtension filepath of 
        True  -> fromJust $ extension filepath 
        False -> filepath

mkFolderName :: String -> String 
mkFolderName s = delete '.' (Data.Char.toUpper <$> s) 
mkFolderEx s = '.' : (Data.Char.toLower <$> s)



main2 :: [Subdirectory] -> Shell [Map [Subdirectory] [File]]
main2 subs  = do 
    case subs of 
        []          -> pure [] 
        (sub : subs') -> do 
            let workingDirectory = ls (fromSub sub) 
            paths                <- getPaths workingDirectory 
            parentSubdirectories <- liftIO $ getSubdirectories paths 
            parentFiles          <- liftIO $ getFiles paths 
            liftM2  (++) (pure [Data.Map.Strict.singleton parentSubdirectories parentFiles]) (main2 subs')




newtype Subdirectory = Subdirectory FilePath deriving (Eq, Show, Ord)
fromSub (Subdirectory fp) = fp

newtype File = File FilePath deriving (Eq, Show, Ord)
fromFile (File fp) = fp

type TreeDict = Map Subdirectory (Shell FilePath) 

data Folder = Folder 
            { folderName  :: String
            , folderFiles :: Set File 
            } deriving (Show, Eq, Ord)

makeTrees :: [Subdirectory] -> TreeDict
makeTrees subs = Data.Map.Strict.fromList ((\sub -> (sub , lstree $ fromSub sub)) <$> subs)

lookupTreeVals :: [Subdirectory] -> TreeDict -> [Shell FilePath]
lookupTreeVals subs treedict = 
    case subs of 
        []            -> [] 
        (sub : subs') -> (treedict ! sub) : (lookupTreeVals subs' treedict)

getSubdirectories :: [FilePath] -> IO [Subdirectory]
getSubdirectories dir = do 
            dirs <- (filterM testdir dir)
            pure $ Subdirectory <$> dirs 

getFiles :: [FilePath] -> IO [File]
getFiles dir = do 
    files <- (filterM testfile dir)
    pure $ File <$> files


-------------------------------------------
-- Folding functions on stream

getPaths :: Shell FilePath -> Shell [FilePath]
getPaths fp = reduce foldLSOut fp   

foldLSOut = Fold step initial extract

step :: [FilePath] -> FilePath -> [FilePath]
step acc path = (Prelude.lines path) ++ acc
initial :: [FilePath]
initial = [] 
extract :: [FilePath] -> [FilePath]
extract = id 

 
---------------------------------------

    


