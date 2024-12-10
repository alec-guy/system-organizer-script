module Main where

import Data.Text as T hiding (find)
-- Objective One 
-- Finds all files in a directory and its subdirectories. 
-- Objective Two 
-- Sort these files into folders based on their extension

import Turtle.Shell (sh, Shell(..), select, view, reduce)
import Turtle.Prelude (ls, find, testdir, testfile, lstree, mkdir)
import Turtle (splitDirectories, liftIO, Fold(..), hasExtension, extension)
import Control.Monad (filterM, mapM, sequence, liftM2, sequence_)
import Data.Map.Strict (fromList, Map(..), (!), size, singleton, elems)
import Data.List (nub, partition, delete)
import Data.Maybe (fromJust)
import Data.Char (toUpper)
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
  let files = ((getOnlyFiles mapSubsFiles) ++ (parentFiles) ++ (childrenFiles))
            where getOnlyFiles mapSF = Prelude.concat (Prelude.concat (elems <$> mapSF))     
      folders = nub $ sortFilesByExtension files 
  liftIO $ makeFolders folders 

makeFolders :: [Folder] -> IO ()
makeFolders folders = sequence_ (makeFolder <$> folders) 
     where makeFolder :: Folder -> IO () 
           makeFolder folder = do 
            mkdir (folderName folder)

sortFilesByExtension :: [File] -> [Folder]
sortFilesByExtension files = 
    case files of 
        []       -> [] 
        (f : fs) -> case (Data.List.partition (hasSameExtension f) fs) of 
                     (listThatDoes, listThatDoesn't) -> 
                                       ( Folder 
                                         { folderName  = "./" ++ (mkFolderName (extension' f)) ++ "Root-Beer-Guy"
                                         , folderFiles = listThatDoes
                                         }
                                       ) : (sortFilesByExtension listThatDoesn't)
hasSameExtension :: File -> File -> Bool 
hasSameExtension file1 file2 = (extension' file1) == (extension' file2)

extension' :: File -> String 
extension' (File filepath) = 
    case hasExtension filepath of 
        True  -> fromJust $ extension filepath 
        False -> filepath

mkFolderName :: String -> String 
mkFolderName s = delete '.' (Data.Char.toUpper <$> s) 



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

newtype File = File FilePath deriving (Eq, Show)

type TreeDict = Map Subdirectory (Shell FilePath) 

data Folder = Folder 
            { folderName  :: String
            , folderFiles :: [File]
            } deriving (Show, Eq)

makeTrees :: [Subdirectory] -> TreeDict
makeTrees subs = fromList ((\sub -> (sub , lstree $ fromSub sub)) <$> subs)

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

    


