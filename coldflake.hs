{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), arr)
import Control.Monad (forM_)
import Data.Monoid (mempty)
import Text.Pandoc (WriterOptions(..))

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

    -- Tags
    create "tags" $
      requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $
      gsubRoute "C\\+\\+" (const "Cpptag") `composeRoutes` setExtension "html"
    metaCompile $ require_ "tags"
      >>> arr tagsMap
      >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))
        
    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration
            
    -- Read templates
    match "templates/*" $ compile templateCompiler

    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "Posts")
        >>> arr (setField "bodyclass" "postlist")
        >>> setFieldPageList recentFirst
                "templates/postitem.html" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
            

    -- Render pages with relative url's
    forM_ ["about.md","colophon.md","404.md"] $ \p ->
        match p $ do
            route $ setExtension ".html"
            compile $ blogCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

  where
      renderTagCloud' :: Compiler (Tags String) String
      renderTagCloud' = renderTagCloud tagIdentifier 50 250 

      tagIdentifier :: String -> Identifier (Page String)
      tagIdentifier = fromCapture "tags/*"
    
      description = "Exploring and learning in the dazzling array of fascintating software technologies"
      keywords = "marcmo, haskell, hakyll, programming, ruby, rake, bash, linux"

      -- Useful combinator here
      xs --> f = mapM_ (\p -> match p $ f) xs

      -- Completely static
      copy = route idRoute >> compile copyFileCompiler

      -- CSS directories
      css = route (setExtension "css") >> compile compressCssCompiler

      post = do
        route   $ setExtension ".html"
        compile $ blogCompiler
          >>> arr (renderDateField "date" "%Y-%m-%d" "Date unknown")
          >>> arr (setField "bodyclass" "post")
          >>> renderTagsField "prettytags" (fromCapture "tags/*")
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
            >>> requireA "tags" (setFieldA "tags" (renderTagCloud'))
            >>> setFieldPageList (take 10 . recentFirst)
                    "templates/postitem.html" "posts" "posts/*"
            >>> applyTemplateCompiler "templates/index.html"
            >>> applyTemplateCompiler "templates/default.html"
      
      
makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
        >>> pageListCompiler recentFirst "templates/postitem.html"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" $ "Posts tagged " ++ tag)
        >>> arr (setField "description" $ "View all posts tagged with " ++ tag)
        >>> arr (setField "keywords" $ "tags, " ++ tag)
        >>> arr (setField "bodyclass" "postlist")
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

