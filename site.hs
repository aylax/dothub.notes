--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.List            (isSuffixOf, isPrefixOf)
import qualified Text.Pandoc          as Pandoc
import           Text.Pandoc.SideNote (usingSideNotes)
import           Text.Pandoc.Walk     (walk)
import           System.FilePath      (takeBaseName, takeDirectory, (</>))

------------------------------    SITE.H    ------------------------------------
main :: IO ()
main = hakyll $ do
------------------------------    ASSETS    ------------------------------------
    match "src/assets/images/*" $ do
        route $ customRoute $ drop 11 . toFilePath
        compile copyFileCompiler

    match "src/assets/css/*.css" $ do
        route $ customRoute $ drop 11 . toFilePath
        compile compressCssCompiler

------------------------------    THEMES    ------------------------------------
    match "themes/tufte/tufte.css" $ do
        route $ customRoute $ drop 7 . toFilePath
        compile compressCssCompiler

    match "themes/tufte/et-book/*/*" $ do
        route $ customRoute $ drop 13 . toFilePath
        compile copyFileCompiler

------------------------------ STYLESHEET --------------------------------------
    create ["stylesheet.css"] $ do
        route idRoute
        compile $ do
            theme <- load "themes/tufte/tufte.css"
            asset <- loadAll "src/assets/css/*.css"
            makeItem $ unlines $ map itemBody $ theme : asset

------------------------------    POSTS     ------------------------------------
    match "src/pages/*.org" $ do
        route $ niceRoute
        compile $ pandocExtCompiler
            >>= loadAndApplyTemplate "templates/default.html" dateCtx
            >>= relativizeUrls

    match "src/posts/*.org" $ do
        route $ niceRoute
        compile $ pandocExtCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" dateCtx
            >>= relativizeUrls
    
    create ["posts"] $ do
        route $ niceRoute
        compile $ do
            posts <- recentFirst =<< loadAll "src/posts/*"
            let ctx =
                    listField "posts" dateCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    baseCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "src/index.html" $ do
        route $ constRoute "index.html"
        compile $ do
            posts <- loadAll "src/posts/*"
            shows <- take 5 <$> recentFirst posts
            let indexCtx =
                    listField "posts" dateCtx (return shows) `mappend`
                    baseCtx
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let ctx = dateCtx `mappend` constField "description" "description"
            posts <- take 10 <$> (recentFirst =<< loadAllSnapshots "src/posts/*" "content")
            renderAtom atom ctx posts

------------------------------     RSS      ------------------------------------
atom :: FeedConfiguration
atom = FeedConfiguration
    { feedTitle = "aylax's site"
    , feedDescription = "site of aylax"
    , feedAuthorName = "aylax zhou"
    , feedAuthorEmail = "zhoubye@foxmail.com"
    , feedRoot = "https//aylax.github.io"
    }

------------------------------    UTILS     ------------------------------------
pandocExtCompiler :: Compiler (Item String)
pandocExtCompiler = let readerOptions = defaultHakyllReaderOptions
                        writerOptions = defaultHakyllWriterOptions
                    in pandocCompilerWithTransform readerOptions writerOptions (usingSideNotes . shiftHeaders 1)

shiftHeaders :: Int -> Pandoc.Pandoc -> Pandoc.Pandoc
shiftHeaders i p = walk go p
    where
        go (Pandoc.Header l a inl) = Pandoc.Header (l+i) a inl
        go x = x

stripTags :: Context String
stripTags = functionField "stripTags" $ \args item -> case args of
  [s] -> return $ Hakyll.stripTags s
  _   -> error "stripTags only takes one argument"

baseCtx :: Context String
baseCtx = Main.stripTags `mappend` defaultContext

dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y" `mappend` baseCtx

cleanUrl :: String -> String
cleanUrl url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
    where idx = "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanUrl)

niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where createIndexRoute ident = directory </> pageName </> "index.html"
          where p = toFilePath ident
                directory = if "src" `isPrefixOf` dir
                             then drop 4 dir -- 4 == (len "src/")
                             else dir
                dir = takeDirectory p
                bn = takeBaseName p
                pageName = bn
