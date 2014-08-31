{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative((<$>))
import Data.Monoid(mappend)
import Text.Blaze.Html((!),toHtml,toValue)
import Text.Blaze.Html.Renderer.String(renderHtml)
import Text.Blaze.Html5(a)
import Text.Blaze.Html5.Attributes(href)

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Static files
    match ("images/**"
          .||. "js/**"
          .||. "code/**"
          .||. "fonts/**"
          .||. "test.html"
          .||. "404.html") $ do
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
            >>= loadAndApplyTemplate "templates/post.html" (withoutTagcloudCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (withoutTagcloudCtx tags)
            >>= relativizeUrls
            >>= cleanUpUrls

    match "about.md" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (withoutTagcloudCtx tags)
            >>= relativizeUrls

    -- Render posts list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl (withoutTagcloudCtx tags) sorted
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
            list <- applyTemplateList itemTpl (withoutTagcloudCtx tags) sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/index.html" (homeCtx list)
                >>= loadAndApplyTemplate "templates/default.html" (homeCtx list)
                >>= relativizeUrls
                >>= cleanUpUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "<i class=\"icon-tag\"></i> Posts tagged " ++ tag
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
                            tagCtx)
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
renderTagElem = renderTags makeLink unwords
    where makeLink tag url _ _ _ =
            renderHtml $ a ! href (toValue url) $ toHtml tag

cleanUpUrls :: Item String -> Compiler (Item String)
cleanUpUrls item = return $ fmap (withUrls escapeStr) item

escapeStr :: String -> String
escapeStr = concatMap escape
  where escape x
          | x == '+' = "%2B"
          | otherwise = [x]

-- first level contexts
descriptionCtx :: Context String
descriptionCtx =
    constField "blogDescription" blogDescription `mappend`
    defaultContext

-- second level contexts
homeCtx :: String -> Context String
homeCtx list =
    constField "posts" list `mappend`
    constField "title" "Index" `mappend`
    constField "tagcloud" "" `mappend`
    constField "mobileimage" "src=\"../images/snowtrees.jpg\"" `mappend`
    descriptionCtx

postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    constField "mobileimage" "src=\"../images/roundlogo.png\" width=\"130px\"" `mappend`
    descriptionCtx

tagCtx :: Context String
tagCtx =
    constField "mobileimage" "src=\"../images/snowtrees.jpg\"" `mappend`
    descriptionCtx

-- third level contexts
allPostsCtx :: Context String
allPostsCtx =
    constField "title" "All posts" `mappend`
    constField "tagcloud" "" `mappend`
    postCtx

feedCtx :: Context String
feedCtx =
    bodyField "description" `mappend`
    postCtx

withoutTagcloudCtx :: Tags -> Context String
withoutTagcloudCtx tags =
    tagsField "prettytags" tags `mappend`
    constField "tagcloud" "" `mappend`
    postCtx

blogDescription ::  String
blogDescription = "Exploring software technologies"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "colflake blog - RSS feed"
    , feedDescription = "writeup of various programming related topics"
    , feedAuthorName  = "Oliver Mueller"
    , feedAuthorEmail = "oliver.mueller@gmail.com"
    , feedRoot        = "http://blog.coldflake.com"
    }

postList :: Tags
         -> Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- loadAll pattern
    processed <- preprocess' posts
    applyTemplateList postItemTpl (withoutTagcloudCtx tags) processed

