module Main where

import Prelude (print)
import RIO
import qualified RIO.FilePath as Path
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import qualified RIO.Text.Lazy as TL
import qualified RIO.Directory as Dir
import qualified Data.Text.ICU as Regex
import qualified Data.Text.Format as Format
import System.Environment
import Parser

{-
parseDoc :: T.Text -> Doc
parseDoc text = (\(Just x) -> x)
  $ parseNote
  <|> parseTeXSection <|> parseSection <|> parseMdSection
--  <|> parseCopyright
  <|> parsePragma
  <|> Just (Paragraph text)

  where
    genParser :: T.Text -> (T.Text -> Doc) -> Maybe Doc
    genParser regex con = do
      let re = Regex.regex [] regex
      match <- Regex.find re text
      fmap con $ Regex.group 1 match

    parseCopyright :: Maybe Doc
    parseCopyright = do
      let re = Regex.regex [] "(\\(c\\) (.+)\n)+"
      match <- Regex.find re text
      fmap Copyright $ sequence $ map (\n -> Regex.group n match) [1..5]

    parseTeXSection = genParser "\\\\section\\[.+\\]\\{(.+)\\}" Section
    parseSection = genParser "\\*{3}[\\s\\*]+([^\\*]*)[\\s\\*]+\\*{3}" Section
    parseMdSection = genParser "(\\s.+)\n~+~{5}" (Section . T.strip)
    parseNote = genParser "Note \\[(.*)\\](~+~{5})?" Note
    parsePragma = genParser "#\\s(.+)\\s#" Pragma
-}

renderDoc :: Doc -> Maybe T.Text
renderDoc doc = case doc of
  Section t -> Just $ TL.toStrict $ Format.format "# {}" [t]
  Note t -> Just $ TL.toStrict $ Format.format "### Note: {}" [t]
  Pragma t -> Nothing
  Copyright ts -> Just $ TL.toStrict $ TL.unlines $ map (Format.format "> {}" . (\x -> [x])) ts
  Paragraph t -> Just t

getBlockComments :: T.Text -> [T.Text]
getBlockComments text = do
  let re = Regex.regex [] "\\{\\-([\\s\\S]*?)\\-\\}"
  catMaybes $ map (Regex.group 1) $ Regex.findAll re text

isPragma :: T.Text -> Bool
isPragma t = not (T.null t) && T.take 1 t == "#"

data FileType = File | Directory deriving (Show)

isFile :: FileType -> Bool
isFile File = True
isFile _ = False

createIndex :: FilePath -> FilePath -> [(FileType, FilePath)] -> IO ()
createIndex cur base paths = do
  Dir.createDirectoryIfMissing True (Path.joinPath [cur, base])
  writeFileUtf8 (Path.joinPath [cur, base, "README.md"]) $ T.unlines $
    [ ("# " `T.append` T.pack base)
    , ""
    ] ++
    map (\(file, filename) -> TL.toStrict $ Format.format "- [{}]({})" [filename, rewritePath file filename]) paths

  where
    rewritePath file filename = case file of
      File -> Path.joinPath [base, Path.replaceExtension filename ""]
      Directory -> Path.joinPath [base, filename] ++ "/"

getFileType :: FilePath -> FilePath -> IO (Maybe FileType)
getFileType base path = do
  isFile <- Dir.doesFileExist $ Path.joinPath [base, path]
  isDirectory <- Dir.doesDirectoryExist $ Path.joinPath [base, path]
  return $
    if not isFile && not isDirectory then Nothing
    else if isFile then Just File else Just Directory

foldOnDir :: FilePath -> (FilePath -> T.Text -> IO ()) -> (FilePath -> [(FileType, FilePath)] -> IO ()) -> IO ()
foldOnDir base onFile onDir = do
  listDir <- Dir.listDirectory base
  paths <- fmap catMaybes $ forM listDir $ \path -> do
    result <- getFileType base path
    return $ maybe Nothing (\ft -> Just (ft, path)) result
  onDir base paths

  forM_ paths $ \(file, path') -> do
    let path = Path.joinPath [base, path']

    case file of
      File -> do
        when (Path.takeExtension path == ".hs") $ readFileUtf8 path >>= onFile path
      Directory -> do
        foldOnDir path onFile onDir

main :: IO ()
main = runSimpleApp $ liftIO $ do
  (path:_) <- getArgs
  current <- Dir.getCurrentDirectory

  let outDir = "docs"
  Dir.createDirectoryIfMissing True $ Path.joinPath [outDir, path]

  foldOnDir path
    (\filename content -> do
        let path = Path.replaceExtensions (Path.joinPath [outDir, filename]) ".md"
        Dir.createDirectoryIfMissing True (Path.dropFileName path)

        writeFileUtf8 path
          $ T.append (TL.toStrict $ Format.format "[[src]](https://github.com/ghc/ghc/tree/master/{})\n" [T.dropPrefix "ghc/" $ T.pack filename])
          $ T.intercalate "\n\n"
          $ catMaybes $ map (renderDoc <=< resultAsMaybe . parseDoc) $ concat
          $ map (T.splitOn "\n\n")
          $ getBlockComments
          $ preprocess
          $ content)
    (\base paths -> do
        createIndex outDir base paths)

  writeFileUtf8 (Path.joinPath [outDir, "README.md"]) $ T.unlines
    [ "# ghc-docs-book"
    , ""
    , "- [ghc/compiler](ghc/compiler/)"
    ]

  where
    preprocess :: T.Text -> T.Text
    preprocess = T.replace "~~~~~\n" "~~~~~\n\n"

