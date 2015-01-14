import qualified System.Directory as Directory
import qualified System.FilePath.Posix as Posix

getTrainingFilenames = do
  fnames <- Directory.getDirectoryContents "training"
  return $ map ("training/" ++) $ filter ((== ".txt") . Posix.takeExtension) fnames

getTrainingStrings = getTrainingFilenames >>= mapM readFile

main = getTrainingStrings >>= mapM putStrLn
