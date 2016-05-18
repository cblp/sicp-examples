{-# LANGUAGE TemplateHaskell #-}

import           Control.Error    (ExceptT(..), fmapL, runExceptT)
import           Data.Aeson.TH    (defaultOptions, deriveFromJSON)
import           Data.List        (isSuffixOf)
import           Data.Maybe       (catMaybes)
import           Data.Monoid      ((<>))
import           Data.Traversable (for)
import qualified Data.Yaml        as Yaml
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath  ((</>))
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase)

data TestDescription = TestDescription
deriveFromJSON defaultOptions ''TestDescription

main :: IO ()
main = do
    tests <- loadTestsFromDirectory "./examples/"
    defaultMain tests

loadTestsFromDirectory :: FilePath -> IO TestTree
loadTestsFromDirectory dir = do
    files <- getDirectoryContents dir
    testGroup dir . catMaybes <$> for files loadTestsFrom
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
    TestDescription{} <- mkExceptT ((file <>) . (": " <>) . show) $ Yaml.decodeFileEither file
    pure . testCase file $ pure ()

mkExceptT :: Functor m => (e1 -> e2) -> m (Either e1 a) -> ExceptT e2 m a
mkExceptT f = ExceptT . fmap (fmapL f)

runScriptToError :: Monad m => ExceptT String m a -> m a
runScriptToError esma = either error id <$> runExceptT esma
