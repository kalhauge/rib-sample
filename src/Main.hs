{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Clay ((?), Css, em, pc, px, sym)
import qualified Clay as C
import Control.Monad
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Development.Shake
import GHC.Generics (Generic)
import Lucid
import Main.Utf8
-- import Rib (IsRoute, Pandoc)
import Rib (Pandoc)
import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc
import System.FilePath

-- base
import Data.Typeable

-- transformers
import Control.Monad.Writer

-- path
import Path

-- Some definitions

-- | A route is a local path to an destination.
-- The implemenation is explictly hidden from the user to avoid the 
-- user create routes that do not exist.
data Route = 
  Route (Path Abs Dir) (Path Rel File)

-- | We can get the url of a route.
routeUrl :: Route -> Text
routeUrl (Route burl url) =  
  T.pack . toFilePath $ burl Path.</> url

-- | We can use the url directly in a a lucid attribute. Use instead of 
-- 'Lucid.href_'
rref_ :: Route -> Attribute
rref_ = href_  . routeUrl

-- | A top-level monad to control the generation of a Site.
-- It is essentially a Write monad which stores all the routes needed 
-- to generate the site. 
newtype SiteGen s a = 
  SiteGen (WriterT [(Route, (s -> Action TL.Text))] Action a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)  

liftAction :: Action a -> SiteGen s a
liftAction m = SiteGen (lift m)

newtype SiteConfig = SiteConfig 
  { baseUrl :: Path Abs Dir
  } deriving (Typeable)

-- | Create a route
makeRoute :: 
  Path Rel File 
  -- | The relative name of the route
  -> (s -> Action TL.Text) 
  -- | The action required to make the route
  -> SiteGen s Route
makeRoute rel act = do
  -- At SomePoint
  -- Just (SiteConfig { baseUrl }) <- liftAction getShakeExtra
  let route = Route [absdir|/|] rel
  SiteGen (tell [(route, act)])
  return $ route
 
-- | Create an HTML Route
writeHtmlRoute :: 
  Path Rel File 
  -> (s -> Action (Html ()))
  -> SiteGen s Route
writeHtmlRoute r f = 
  makeRoute r ( fmap (Lucid.renderText) . f )

-- | Like the Rib version but over SiteGen.
forEvery :: 
  [Path Rel File] 
  -> (Path Rel File -> SiteGen s a) 
  -> SiteGen s [a]
forEvery paths f = do
  fs <- liftAction $ do 
    input <- Rib.ribInputDir
    fs <- getDirectoryFiles' input paths
    return fs
  forM fs f

 where
  -- | Like `getDirectoryFiles` but works with `Path`
  getDirectoryFiles' :: FilePath -> [Path Rel File] -> Action [Path Rel File]
  getDirectoryFiles' dir (fmap toFilePath -> pat) =
    traverse (liftIO . parseRelFile) 
      =<< getDirectoryFiles dir pat


-- | Get a route to a Static file.
buildStaticFile :: Path Rel File -> SiteGen s Route
buildStaticFile p = do
  makeRoute p . const $ do 
    input <- Rib.ribInputDir
    TL.pack <$> readFile' (input System.FilePath.</> toFilePath p)

newRun :: 
  FilePath 
  -> FilePath 
  -> SiteGen s s 
  -> IO ()
newRun content dest (SiteGen gen) = do
  Rib.run content dest $ do 
    (site, routes) <- runWriterT gen
    void $ forP routes $ \(Route _ path, generator) -> do
      txt <- generator site
      Rib.writeFileCached (toFilePath path) (TL.unpack txt)


-- FROM HERE ON IS THE REAL SAMPLE CODE

-- | Specify Post Data
data Post = Post
  { postTitle       :: Text
  , postDescription :: Maybe Text
  , postRoute       :: Route
  }

-- | Specify the Site Routes
data Site = Site
  { siteStaticReadme :: Route
  , siteIndex :: Route
  , sitePosts :: [ Post ]
  }


-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `content`, from which static files will be read.
-- 2. Directory `dest`, under which target files will be generated.
-- 3. Shake action to run.
--
-- In the shake action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = withUtf8 $ do
  newRun "content" "dest" generateSite

-- | Shake action for generating the static site
generateSite :: SiteGen Site Site
generateSite = do
  -- Copy over the static files
  siteStaticReadme <- 
    buildStaticFile [relfile|static/README.md|]
  
  -- Render the Site Index
  siteIndex <- writePage [relfile|index.html|] $ \Site { sitePosts } -> Page 
    { pageTitle = "My Home Page"
    , pageBody = do
        div_ $ forM_ sitePosts $ \Post { .. } ->
          li_ [class_ "pages"] $ do
            b_ $ a_ [rref_ postRoute] $ toHtml $ postTitle
            renderMarkdown `mapM_` postDescription
    }

  -- Build individual sources, generating .html for each.
  sitePosts <-
    Main.forEvery [[relfile|*.md|]] $ \srcPath -> do
      doc <- liftAction $ 
        Pandoc.parse Pandoc.readMarkdown (toFilePath srcPath)

      let SrcMeta { title, description } = getMeta doc
      postRoute <- writePage srcPath $ \_ -> Page 
        { pageTitle = toHtml title
        , pageBody = 
            article_ (Pandoc.render doc)
        }
      
      pure $ Post 
        { postRoute 
        , postTitle = title
        , postDescription = description
        }

  return $ Site {..}

 where 
  renderMarkdown :: Text -> Html ()
  renderMarkdown =
    Pandoc.render . Pandoc.parsePure Pandoc.readMarkdown
  
  writePage :: Path Rel File -> (Site -> Page) -> SiteGen Site Route
  writePage rel fn = 
    writeHtmlRoute rel (\s -> pure $ renderPage (fn s) s)

data Page = Page
  { pageTitle :: Html ()
  , pageBody  :: Html ()
  }

-- | Define your site HTML here
renderPage :: Page -> Site -> Html ()
renderPage Page { .. } Site { siteIndex } = html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ pageTitle
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    div_ [class_ "header"] $
      a_ [rref_ siteIndex ] "Back to Home"
    h1_ pageTitle
    pageBody

-- | Define your site CSS here
pageStyle :: Css
pageStyle = C.body ? do
  C.margin (em 4) (pc 20) (em 1) (pc 20)
  ".header" ? do
    C.marginBottom $ em 2
  "li.pages" ? do
    C.listStyleType C.none
    C.marginTop $ em 1
    "b" ? C.fontSize (em 1.2)
    "p" ? sym C.margin (px 0)

-- | Metadata in our markdown sources
data SrcMeta
  = SrcMeta
      { title :: Text,
        -- | Description is optional, hence `Maybe`
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: Pandoc -> SrcMeta
getMeta src = case Pandoc.extractMeta src of
  Nothing -> error "No YAML metadata"
  Just (Left e) -> error $ T.unpack e
  Just (Right val) -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v
