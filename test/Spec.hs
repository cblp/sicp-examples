{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Error    (ExceptT(..), fmapL, runExceptT)
import           Data.Aeson.TH    (defaultOptions, deriveFromJSON, fieldLabelModifier)
import           Data.Char        (isSpace)
import           Data.Foldable    (for_)
import           Data.List        (dropWhileEnd, isSuffixOf)
import           Data.Maybe       (catMaybes)
import           Data.Monoid      ((<>))
import           Data.Traversable (for)
import qualified Data.Yaml        as Yaml
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath  (takeBaseName, takeFileName, (</>))
import           System.Process   (readProcess)
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

data LangSpec = LangSpec{ls_in :: String, ls_out :: String}
deriveFromJSON defaultOptions{fieldLabelModifier = drop 3} ''LangSpec

data LangSpecSet = LangSpecSet{lss_haskell :: [LangSpec], lss_ocaml :: [LangSpec]}
deriveFromJSON defaultOptions{fieldLabelModifier = drop 4} ''LangSpecSet

main :: IO ()
main = do
    tests <- loadTestsFromDirectory "./examples/"
    defaultMain tests

loadTestsFromDirectory :: FilePath -> IO TestTree
loadTestsFromDirectory dir = do
    files <- getDirectoryContents dir
    testGroup (takeFileName dir) . catMaybes <$> for files loadTestsFrom
  where
    loadTestsFrom "."   = pure Nothing
    loadTestsFrom ".."  = pure Nothing
    loadTestsFrom fileName  = do
        let filePath = dir </> fileName
        isdir <- doesDirectoryExist filePath
        if isdir then
            Just <$> loadTestsFromDirectory filePath
        else
            if ".yaml" `isSuffixOf` fileName then
                Just <$> testYaml filePath
            else
                pure Nothing

testYaml :: FilePath -> IO TestTree
testYaml file = runScriptToError $ do
    LangSpecSet{lss_haskell, lss_ocaml} <-
        mkExceptT ((file <>) . (": " <>) . show) $ Yaml.decodeFileEither file
    pure $ testGroup (takeBaseName file)
        [ testCase "Haskell"  $ for_ lss_haskell  testHaskellRepl
        , testCase "OCaml"    $ for_ lss_ocaml    testOcamlRepl
        ]

mkExceptT :: Functor m => (e1 -> e2) -> m (Either e1 a) -> ExceptT e2 m a
mkExceptT f = ExceptT . fmap (fmapL f)

runScriptToError :: Monad m => ExceptT String m a -> m a
runScriptToError esma = either error id <$> runExceptT esma

testHaskellRepl :: LangSpec -> IO ()
testHaskellRepl LangSpec{ls_in, ls_out} = do
    outRaw <- readProcess "ghc" ["-e", ls_in] ""
    let out = dropWhileEnd isSpace outRaw
    assertEqual ls_in ls_out out

testOcamlRepl :: LangSpec -> IO ()
testOcamlRepl LangSpec{ls_in, ls_out} = do
    outRaw <- readProcess "ocaml" ["-noprompt"] ls_in
    let out = dropWhileEnd isSpace . unlines . drop 3 $ lines outRaw
    assertEqual ls_in ls_out out
