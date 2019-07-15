{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Posts where

import Import

data Post =
  Post { title :: Text, text :: Text }
  deriving Show

postForm :: Form Post
postForm =
  renderDivs $
    Post <$> areq textField "Title" Nothing <*> areq textField "Text" Nothing

getPostsR :: Handler Html
getPostsR = do
  (widget, enctype) <- generateFormPost postForm
  emptyLayout $ do
    $(widgetFile "posts")

postPostsR :: Handler Html
postPostsR = do
  ((result, widget), enctype) <- runFormPost postForm
  case result of
    FormSuccess _ ->
      redirect PostsR
    _ ->
      emptyLayout $ do
        $(widgetFile "posts")
