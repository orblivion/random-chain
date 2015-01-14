import qualified System.Directory as Directory
import qualified System.FilePath.Posix as Posix

main = do
  fnames <- Directory.getDirectoryContents "training"
  let desiredFnames = filter ((== ".txt") . Posix.takeExtension) fnames
  mapM putStrLn desiredFnames
