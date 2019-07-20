{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Posts where

import Import

postForm :: Form Post
postForm =
  renderDivs $
    Post <$> areq textField "Title" Nothing <*> areq textareaField "Text" Nothing

getPostsR :: Handler Html
getPostsR = do
  (widget, enctype) <- generateFormPost postForm
  allPosts :: [Entity Post] <- runDB $ selectList [] []
  emptyLayout $ do
    $(widgetFile "posts")

postPostsR :: Handler Html
postPostsR = do
  ((result, widget), enctype) <- runFormPost postForm
  allPosts :: [Entity Post] <- runDB $ selectList [] []
  case result of
    FormSuccess post -> do
      _ <- runDB $ insert post
      redirect PostsR
    _ ->
      emptyLayout $ do
        $(widgetFile "posts")
