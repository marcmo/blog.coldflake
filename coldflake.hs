{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Functor ((<$>))
import Control.Applicative((<$>),(<*>))
import Data.List (isPrefixOf,intercalate,intersperse)
import Data.Monoid (mappend)
import Data.Text (pack, unpack, replace, empty)
import Text.Blaze.Html (toHtml,toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as BlazeHtml
import qualified Text.Blaze.Html5.Attributes as BlazeAttr
import Data.Char

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Static files
    match ("images/**" .||. "js/**" .||. "code/**" .||. "test.html") $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    match "css/*.scss" $ do
            route   $ setExtension "css"
            compile $ getResourceString >>=
                withItemBody (unixFilter "sass" ["-s", "--scss"]) >>=
                return . fmap compressCss

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= (externalizeUrls $ feedRoot feedConfiguration)
            >>= saveSnapshot "content"
            >>= (unExternalizeUrls $ feedRoot feedConfiguration)
            >>= loadAndApplyTemplate "templates/default.html" (tagsCtx tags)
            >>= relativizeUrls
            >>= cleanUpUrls

    -- Render posts list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl (tagsCtx tags) sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/posts.html" allPostsCtx
                >>= loadAndApplyTemplate "templates/default.html" allPostsCtx
                >>= relativizeUrls
                >>= cleanUpUrls

    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- take 10 <$> recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl (tagsCtx tags) sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/index.html" (homeCtx tags list)
                >>= loadAndApplyTemplate "templates/default.html" (homeCtx tags list)
                >>= relativizeUrls
                >>= cleanUpUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            tagList <- renderTagElem tags
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title `mappend`
                            constField "body" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                        (constField "title" title `mappend`
                            constField "tagcloud" tagList `mappend`
                            descriptionCtx)
                >>= relativizeUrls
                >>= cleanUpUrls


    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/*" "content"
            sorted <- take 10 <$> recentFirst posts
            renderRss feedConfiguration feedCtx sorted

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/*" "content"
            sorted <- take 10 <$> recentFirst posts
            renderAtom feedConfiguration feedCtx sorted

    -- Read templates
    match "templates/*" $ compile templateCompiler

renderTagElem :: Tags -> Compiler String
renderTagElem = renderTags makeLink (intercalate " ")
    where
        makeLink tag url count _ _ = renderHtml $
            BlazeHtml.a ! BlazeAttr.href (toValue url) $ toHtml tag

cleanUpUrls :: Item String -> Compiler (Item String)
cleanUpUrls item = return $ fmap (withUrls escapeStr) item

escape x
      | x == '+' = "%2B"
      | otherwise = [x]
escapeStr :: String -> String
escapeStr = concatMap escape

descriptionCtx :: Context String
descriptionCtx =
    constField "description" description `mappend`
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "description" description `mappend`
    descriptionCtx

allPostsCtx :: Context String
allPostsCtx =
    constField "title" "All posts" `mappend`
    constField "tagcloud" "" `mappend`
    postCtx

homeCtx :: Tags -> String -> Context String
homeCtx tags list =
    constField "posts" list `mappend`
    constField "title" "Index" `mappend`
    constField "description" description `mappend`
    constField "tagcloud" "" `mappend`
    descriptionCtx

description = "Exploring and learning in the dazzling array of fascintating software technologies"

feedCtx :: Context String
feedCtx =
    bodyField "description" `mappend`
    postCtx

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "prettytags" tags `mappend`
    constField "tagcloud" "" `mappend`
    postCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "colflake blog - RSS feed"
    , feedDescription = "writeup of various programming related topics"
    , feedAuthorName  = "Oliver Mueller"
    , feedAuthorEmail = "oliver.mueller@gmail.com"
    , feedRoot        = "http://blog.coldflake.com"
    }

externalizeUrls :: String -> Item String -> Compiler (Item String)
externalizeUrls root item = return $ fmap (externalizeUrlsWith root) item

externalizeUrlsWith :: String -- ^ Path to the site root
                    -> String -- ^ HTML to externalize
                    -> String -- ^ Resulting HTML
externalizeUrlsWith root = withUrls ext
  where
    ext x = if isExternal x then x else root ++ x

-- TODO: clean me
unExternalizeUrls :: String -> Item String -> Compiler (Item String)
unExternalizeUrls root item = return $ fmap (unExternalizeUrlsWith root) item

unExternalizeUrlsWith :: String -- ^ Path to the site root
                      -> String -- ^ HTML to unExternalize
                      -> String -- ^ Resulting HTML
unExternalizeUrlsWith root = withUrls unExt
  where
    unExt x = if root `isPrefixOf` x then unpack $ replace (pack root) empty (pack x) else x

postList :: Tags
         -> Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- loadAll pattern
    processed <- preprocess' posts
    applyTemplateList postItemTpl (tagsCtx tags) processed

