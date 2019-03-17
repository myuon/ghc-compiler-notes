{-# LANGUAGE CPP #-}

module GHC.Compiler.Utils.HeaderOptions where

import           Bag                      ( unitBag )
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                ( intercalate )
import           Data.Maybe               ( catMaybes )
import           Data.Version
import           DynFlags
import           ErrUtils
import           Exception
import           FastString
import           FileCleanup
import           GHC.Compiler.Utils.Lexer
import           HeaderInfo
import           HscTypes
import           Lexer
import           Module
import           Outputable
import           Packages
import           Panic
import           Prelude                  hiding ( (<>) )
import           SrcLoc
import           StringBuffer
import           SysTools
import           System.Directory
import           System.FilePath
import qualified System.IO.Temp           as Temp
import           Util
import qualified GHC.LanguageExtensions   as LangExt
import           Data.Functor


-- See https://hackage.haskell.org/package/ghc-8.6.1/docs/src/DriverPipeline.html#line-917
parseDynFlagsFromHsFileHead :: MonadIO m => DynFlags -> FilePath -> m DynFlags
parseDynFlagsFromHsFileHead dflags inputFn = do
  let dflags0 = dflags
        `gopt_unset` Opt_Haddock
        `gopt_unset` Opt_KeepRawTokenStream

  srcOpts <- liftIO $ getOptionsFromFile dflags0 inputFn
  (dflags1, unhandledFlags, _) <- parseDynamicFilePragma dflags srcOpts
  checkProcessArgsResult dflags unhandledFlags

  pure dflags1


runParserMayPreprocessFromFile
  :: MonadIO m
  => Parser a
  -> DynFlags
  -> FilePath
  -> m (Either ParseFailed a)
runParserMayPreprocessFromFile p dflags fn = do
  dflagsWithOptions <- parseDynFlagsFromHsFileHead dflags fn
  ebuf <- if LangExt.Cpp `xopt` dflagsWithOptions
    then liftIO $ Temp.withTempDirectory
      (sTmpDir $ settings dflagsWithOptions)
      "GHC-Compiler-Notes-runParserMayPreprocessFromFile"
      \tdir -> do
        let outFn = tdir </> takeFileName fn
        r <- (doCpp dflagsWithOptions True fn outFn *> pure Nothing)
          `catch` \(e :: GhcException) -> pure $ Just e
        case r of
          Just e  -> pure $ Left e
          Nothing -> do
            -- avoid lazy I/O
            s <- readFile outFn
            length s `seq` (pure $ Right $ stringToStringBuffer s)
    else do
      s <- liftIO $ readFile fn
      pure $ Right $ stringToStringBuffer s
  case ebuf of
    -- FIXME: to ParseFailed
    Left  e   -> do
      liftIO $ print e
      pure $ Left $ ParseFailed []
    Right buf -> pure $ runParser p dflagsWithOptions buf loc
  where
    loc = mkRealSrcLoc (mkFastString fn) 1 1


#include "ghcplatform.h"

-- TODO: To use cpphs
-- See https://hackage.haskell.org/package/ghc-8.6.1/docs/src/DriverPipeline.html#doCpp
doCpp :: DynFlags -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw input_fn output_fn = do
  let hscpp_opts = picPOpts dflags
  let cmdline_include_paths = includePaths dflags

  pkg_include_dirs <- getPackageIncludePath dflags []
  let include_paths_global = foldr (\x xs -> ("-I" ++ x) : xs)
                                   []
                                   (includePathsGlobal cmdline_include_paths ++ pkg_include_dirs)
  let include_paths_quote =
        foldr (\x xs -> ("-iquote" ++ x) : xs) [] (includePathsQuote cmdline_include_paths)
  let include_paths = include_paths_quote ++ include_paths_global

  let verbFlags = getVerbFlags dflags

  let cpp_prog args
        | raw = SysTools.runCpp dflags args
        | otherwise = SysTools.runCc dflags (SysTools.Option "-E" : args)

  let target_defs =
        [ "-D" ++ HOST_OS ++ "_BUILD_OS"
        , "-D" ++ HOST_ARCH ++ "_BUILD_ARCH"
        , "-D" ++ TARGET_OS ++ "_HOST_OS"
        , "-D" ++ TARGET_ARCH ++ "_HOST_ARCH"
        ]
  -- remember, in code we *compile*, the HOST is the same our TARGET,
  -- and BUILD is the same as our HOST.
  let sse_defs
        =  ["-D__SSE__" | isSseEnabled dflags]
        ++ ["-D__SSE2__" | isSse2Enabled dflags]
        ++ ["-D__SSE4_2__" | isSse4_2Enabled dflags]

  let avx_defs
        = ["-D__AVX__" | isAvxEnabled dflags]
        ++ ["-D__AVX2__" | isAvx2Enabled dflags]
        ++ ["-D__AVX512CD__" | isAvx512cdEnabled dflags]
        ++ ["-D__AVX512ER__" | isAvx512erEnabled dflags]
        ++ ["-D__AVX512F__" | isAvx512fEnabled dflags]
        ++ ["-D__AVX512PF__" | isAvx512pfEnabled dflags]

  backend_defs <- getBackendDefs dflags

  let th_defs = ["-D__GLASGOW_HASKELL_TH__"]
  -- Default CPP defines in Haskell source
  ghcVersionH <- getGhcVersionPathName dflags
  let hsSourceCppOpts = ["-include", ghcVersionH]

  -- MIN_VERSION macros
  let uids = explicitPackages (pkgState dflags)
      pkgs = catMaybes (map (lookupPackage dflags) uids)
  mb_macro_include <-
    if not (null pkgs) && gopt Opt_VersionMacros dflags
      then do
        macro_stub <- newTempName dflags TFL_CurrentModule "h"
        writeFile macro_stub (generatePackageVersionMacros pkgs)
        -- Include version macros for every *exposed* package.
        -- Without -hide-all-packages and with a package database
        -- size of 1000 packages, it takes cpp an estimated 2
        -- milliseconds to process this file. See Trac #10970
        -- comment 8.
        return [SysTools.FileOption "-include" macro_stub]
      else return []

  cpp_prog
    (  map SysTools.Option verbFlags
    ++ map SysTools.Option include_paths
    ++ map SysTools.Option hsSourceCppOpts
    ++ map SysTools.Option target_defs
    ++ map SysTools.Option backend_defs
    ++ map SysTools.Option th_defs
    ++ map SysTools.Option hscpp_opts
    ++ map SysTools.Option sse_defs
    ++ map SysTools.Option avx_defs
    ++ mb_macro_include
    -- Set the language mode to assembler-with-cpp when preprocessing. This
    -- alleviates some of the C99 macro rules relating to whitespace and the hash
    -- operator, which we tend to abuse. Clang in particular is not very happy
    -- about this.
    ++
      [ SysTools.Option "-x"
      , SysTools.Option "assembler-with-cpp"
      , SysTools.Option input_fn
      -- We hackily use Option instead of FileOption here, so that the file
      -- name is not back-slashed on Windows.  cpp is capable of
      -- dealing with / in filenames, so it works fine.  Furthermore
      -- if we put in backslashes, cpp outputs #line directives
      -- with *double* backslashes.   And that in turn means that
      -- our error messages get double backslashes in them.
      -- In due course we should arrange that the lexer deals
      -- with these \\ escapes properly.
      , SysTools.Option "-o"
      , SysTools.FileOption "" output_fn
      ]
    )

-- See https://hackage.haskell.org/package/ghc-8.6.1/docs/src/HeaderInfo.html#getOptions%27
getOptionsFromTokenStream :: DynFlags -> Parser [Located String]
getOptionsFromTokenStream dflags = parseToksWithFullArg
  where
    pToken = lexer False pure

    getToken (L _loc tok) = tok
    -- getLoc (L loc _tok) = loc

    parseToksWithFullArg = do
      arg1 <- pToken
      parseToksWith1Arg arg1

    parseToksWith1Arg arg1 = do
      arg2 <- pToken
      parseToks arg1 arg2

    parseToks open close
      | IToptions_prag str <- getToken open
      , ITclose_prag       <- getToken close
      = case toArgs str of
        Left err   -> panic ("getOptions'.parseToks: " ++ err)
        Right args -> (map (L (getLoc open)) args ++)
          <$> parseToksWithFullArg
    parseToks open close
      | ITinclude_prag str <- getToken open
      , ITclose_prag       <- getToken close
      = (map (L (getLoc open)) ["-#include", removeSpaces str] ++)
        <$> parseToksWithFullArg
    parseToks open close
      | ITdocOptions str <- getToken open
      , ITclose_prag     <- getToken close
      = (map (L (getLoc open)) ["-haddock-opts", removeSpaces str] ++)
        <$> parseToksWithFullArg
    parseToks open rarg1
      | ITlanguage_prag <- getToken open
      = parseLanguage rarg1
    parseToks comment rarg1 -- Skip over comments
      | isComment (getToken comment)
      = parseToksWith1Arg rarg1
    parseToks _ _ = pure []

    parseLanguage (L loc1 (ITconid fs))
      =   (checkExtension dflags (L loc1 fs) :)
      <$> do
        pToken >>= \case
          L _loc2 ITcomma -> pToken >>= parseLanguage
          L _loc2 ITclose_prag -> pToken >>= parseLanguage
          L loc2 _ -> languagePragParseError dflags loc2
          -- [] -> panic "getOptions'.parseLanguage(1) went past eof token"
    parseLanguage tok = languagePragParseError dflags (getLoc tok)
    {-
    parseLanguage []
        = panic "getOptions'.parseLanguage(2) went past eof token"
    -}

    isComment :: Token -> Bool
    isComment c = case c of
      (ITlineComment{})     -> True
      (ITblockComment{})    -> True
      (ITdocCommentNext{})  -> True
      (ITdocCommentPrev{})  -> True
      (ITdocCommentNamed{}) -> True
      (ITdocSection{})      -> True
      _                     -> False

languagePragParseError :: DynFlags -> SrcSpan -> a
languagePragParseError dflags loc = throw $ mkSrcErr $ unitBag $
  (mkPlainErrMsg dflags loc $ vcat
    [ text "Cannot parse LANGUAGE pragma"
    , text "Expecting comma-separated list of language options,"
    , text "each starting with a capital letter"
    , nest 2 (text "E.g. {-# LANGUAGE TemplateHaskell, GADTs #-}")
    ]
  )

checkExtension :: DynFlags -> Located FastString -> Located String
checkExtension dflags (L l ext)
  -- Checks if a given extension is valid, and if so returns
  -- its corresponding flag. Otherwise it throws an exception.
  = let ext' = unpackFS ext in
    if ext' `elem` supportedLanguagesAndExtensions
      then L l ("-X" ++ ext')
      else unsupportedExtnError dflags l ext'

unsupportedExtnError :: DynFlags -> SrcSpan -> String -> a
unsupportedExtnError dflags loc unsup = throw $ mkSrcErr $ unitBag $ mkPlainErrMsg dflags loc $
  text "Unsupported extension: " <> text unsup $$
  if null suggestions
    then Outputable.empty
    else text "Perhaps you meant" <+> quotedListWithOr (map text suggestions)
  where
    suggestions = fuzzyMatch unsup supportedLanguagesAndExtensions

-- See https://hackage.haskell.org/package/ghc-8.6.1/docs/src/DriverPipeline.html#getBackendDefs
getBackendDefs :: DynFlags -> IO [String]
getBackendDefs dflags
  | hscTarget dflags == HscLlvm = do
    llvmVer <- figureLlvmVersion dflags
    return $ case llvmVer of
      Just n -> ["-D__GLASGOW_HASKELL_LLVM__=" ++ format n]
      _      -> []
  where
    format (major, minor)
      | minor >= 100 = error "getBackendDefs: Unsupported minor version"
      | otherwise = show $ (100 * major + minor :: Int) -- Contract is Int
getBackendDefs _ = return []

-- See https://hackage.haskell.org/package/ghc-8.6.1/docs/src/DriverPipeline.html#getGhcVersionPathName
getGhcVersionPathName :: DynFlags -> IO FilePath
getGhcVersionPathName dflags = do
  candidates <- case ghcVersionFile dflags of
    Just path -> return [path]
    Nothing   -> (map (</> "ghcversion.h"))
      <$> (getPackageIncludePath dflags [toInstalledUnitId rtsUnitId])

  found <- filterM doesFileExist candidates
  case found of
    []    -> throwGhcExceptionIO (InstallationError ("ghcversion.h missing; tried: "
                                                     ++ intercalate ", " candidates))
    (x:_) -> return x

-- See https://hackage.haskell.org/package/ghc-8.6.1/docs/src/DriverPipeline.html#generatePackageVersionMacros
generatePackageVersionMacros :: [PackageConfig] -> String
generatePackageVersionMacros pkgs = concat
  -- Do not add any C-style comments. See Trac #3389.
  [ generateMacros "" pkgname version
  | pkg <- pkgs
  , let
      version = packageVersion pkg
      pkgname = map fixchar (packageNameString pkg)
  ]

-- See https://hackage.haskell.org/package/ghc-8.6.1/docs/src/DriverPipeline.html#fixchar
fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c   = c

-- See https://hackage.haskell.org/package/ghc-8.6.1/docs/src/DriverPipeline.html#generateMacros
generateMacros :: String -> String -> Version -> String
generateMacros prefix name version = concat
  ["#define ", prefix, "VERSION_",name," ",show (showVersion version),"\n"
  ,"#define MIN_", prefix, "VERSION_",name,"(major1,major2,minor) (\\\n"
  ,"  (major1) <  ",major1," || \\\n"
  ,"  (major1) == ",major1," && (major2) <  ",major2," || \\\n"
  ,"  (major1) == ",major1," && (major2) == ",major2," && (minor) <= ",minor,")"
  ,"\n\n"
  ]
  where
    unconsVersionBranch []     = ("0", [])
    unconsVersionBranch (v:vs) = (v, vs)

    (major1, (major2, (minor, _))) =
      let
        vs = map show $ versionBranch version
      in unconsVersionBranch vs <&> \vs0 -> unconsVersionBranch vs0 <&> unconsVersionBranch
