{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Posts where

import Import

postForm :: UserId -> Form Post
postForm userId =
  renderDivs $
    Post <$>
    areq textField "Title" Nothing <*>
    areq textareaField "Text" Nothing <*>
    areq hiddenField "" (Just userId)

getPostsR :: Handler Html
getPostsR = do
  userId <- fmap fst $ requireAuthPair
  (widget, enctype) <- generateFormPost $ postForm userId
  allPosts :: [Entity Post] <- runDB $ selectList [] [ Desc PostId ]
  emptyLayout $ do
    $(widgetFile "posts")

postPostsR :: Handler Html
postPostsR = do
  userId <- fmap fst $ requireAuthPair
  ((result, widget), enctype) <- runFormPost $ postForm userId
  allPosts :: [Entity Post] <- runDB $ selectList [] [ Desc PostId ]
  case result of
    FormSuccess post -> do
      _ <- runDB $ insert post
      redirect PostsR
    _ ->
      emptyLayout $ do
        $(widgetFile "posts")

deletePostR :: PostId -> Handler Html
deletePostR postId = do
  _ <- runDB $ delete postId
  redirect PostsR
