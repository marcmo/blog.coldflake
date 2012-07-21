{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where

import Prelude hiding (id)
import Control.Monad (forM_)
import Data.Monoid (mempty)
import Text.Pandoc (WriterOptions(..))
import Data.List(intercalate,intersperse)
import qualified Text.Blaze.Html5 as H
import Text.Blaze ((!), toValue)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.String(renderHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Control.Category (id)
import Control.Arrow ((>>>), arr, (&&&), (***), (<<^), returnA)
import Data.Maybe (catMaybes, fromMaybe)

import Hakyll

main :: IO ()
main = hakyll $ do
    ["images/**"]   --> copy
    ["js/**"]   --> copy
    ["code/**"]   --> copy
    ["test.html"]   --> copy

    ["posts/*"] --> post 
    ["css/*"] --> css 
    ["index.html"] --> index
    ["posts.html"] --> allposts

    -- Tags
    create "tags" $
      requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension "html"
    metaCompile $ require_ "tags"
      >>> arr tagsMap
      >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagListCompiler t p)))
        
    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration
            
    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render pages with relative url's
    forM_ ["about.md","colophon.md","404.md"] $ \p ->
        match p $ do
            route $ setExtension ".html"
            compile $ blogCompiler
                >>> arr (setField "tagcloud" "")
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

  where
      renderTagCloud' :: Compiler (Tags String) String
      renderTagCloud' = renderMyTags tagIdentifierEscaped

      tagIdentifier :: String -> Identifier (Page String)
      tagIdentifier = fromCapture "tags/*"
      tagIdentifierEscaped = fromCapture "tags/*" . escapeStr
    
      description = "Exploring and learning in the dazzling array of fascintating software technologies"
      keywords = "marcmo, haskell, hakyll, programming, ruby, rake, bash, linux"

      escape x 
            | x == '+' = "%2B"
            | otherwise = x:[]
      escapeStr = concatMap escape

      -- Useful combinator here
      xs --> f = mapM_ (`match` f) xs

      -- Completely static
      copy = route idRoute >> compile copyFileCompiler

      -- CSS directories
      css = route (setExtension "css") >> compile sass

      sass :: Compiler Resource String
      sass = getResourceString >>> unixFilter "sass" ["-s","--scss"]
                              >>> arr compressCss
      
      post = do
        route   $ setExtension ".html"
        compile $ blogCompiler
            >>> arr (renderDateField "date" "%Y-%m-%d" "Date unknown")
            >>> arr (setField "bodyclass" "post")
            >>> arr (setField "tagcloud" "")
            >>> arr (changeField "url" escapeStr)
            >>> renderTagsField "prettytags" tagIdentifierEscaped
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

      index = do
        route idRoute
        create "index.html" $ constA mempty
            >>> arr (setField "title" "coldflake blog")
            >>> arr (setField "description" description)
            >>> arr (setField "keywords" keywords)
            >>> arr (setField "bodyclass" "default")
            >>> arr (setField "tagcloud" "")
            >>> setFieldPageList (take 10 . recentFirst)
                    "templates/postitem.html" "posts" "posts/*"
            >>> applyTemplateCompiler "templates/index.html"
            >>> applyTemplateCompiler "templates/default.html"

      allposts = do
        route idRoute
        create "posts.html" $ constA mempty
            >>> arr (setField "title" "Posts")
            >>> arr (setField "bodyclass" "postlist")
            >>> arr (setField "tagcloud" "")
            >>> setFieldPageList recentFirst
                    "templates/postitem.html" "posts" "posts/*"
            >>> applyTemplateCompiler "templates/posts.html"
            >>> applyTemplateCompiler "templates/default.html"
      
      
      makeTagListCompiler :: String -> [Page String] -> Compiler () (Page String)
      makeTagListCompiler tag posts =
        constA posts
            >>> pageListCompiler recentFirst "templates/postitem.html"
            >>> arr (copyBodyToField "posts" . fromBody)
            >>> arr (setField "title" $ "Posts tagged " ++ tag)
            >>> arr (setField "description" $ "View all posts tagged with " ++ tag)
            >>> arr (setField "keywords" $ "tags, " ++ tag)
            >>> arr (setField "bodyclass" "postlist")
            >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
            >>> applyTemplateCompiler "templates/posts.html"
            >>> applyTemplateCompiler "templates/default.html"

blogCompiler :: Compiler Resource (Page String)
blogCompiler = pageCompilerWith defaultHakyllParserState opts
  where opts = defaultHakyllWriterOptions
                 { writerHtml5 = True
                 , writerTableOfContents = True
                 , writerLiterateHaskell = False
                 }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Conflating Bits"
    , feedDescription = "writeup of various programming related topics"
    , feedAuthorName = "Oliver Mueller"
    , feedRoot = "http://blog.coldflake.com"
    }

renderMyTags :: (String -> Identifier (Page a)) -> Compiler (Tags a) String
renderMyTags makeUrl = proc (Tags tags) -> do
    tags' <- mapCompiler ((id &&& (getRouteFor <<^ makeUrl)) *** arr length)
                -< tags
    returnA -< renderHtml $ mapM_ toHtml (intersperse (toHtml (" " :: String)) (map makeItem tags'))

makeItem :: ((String, Maybe FilePath), Int) -> H.Html
makeItem ((tag, maybeUrl), count) =
      H.a ! A.href (toValue url) $ toHtml tag
        where url = toUrl $ fromMaybe "/" maybeUrl

