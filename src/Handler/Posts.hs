{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Posts where

import           Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

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
  allPosts <- runDB
         $ E.select
         $ E.from $ \(post `E.InnerJoin` user) -> do
              E.on $ post ^. PostUserId E.==. user ^. UserId
              E.orderBy [E.desc (post ^. PostId)]
              return
                  ( post ^. PostId
                  , post ^. PostUserId
                  , post ^. PostTitle
                  , post ^. PostText
                  , user ^. UserIdent
                  )
  emptyLayout $ do
    $(widgetFile "posts")

getApiPostsR :: Handler Value
getApiPostsR = do
  userId <- fmap fst $ requireAuthPair
  (widget, enctype) <- generateFormPost $ postForm userId
  allPosts <- runDB
         $ E.select
         $ E.from $ \(post `E.InnerJoin` user) -> do
              E.on $ post ^. PostUserId E.==. user ^. UserId
              E.orderBy [E.desc (post ^. PostId)]
              return (post, user)
  return $ object [ "posts" .= allPosts ]

postPostsR :: Handler Html
postPostsR = do
  userId <- fmap fst $ requireAuthPair
  ((result, widget), enctype) <- runFormPost $ postForm userId
  allPosts <- runDB
         $ E.select
         $ E.from $ \(post `E.InnerJoin` user) -> do
              E.on $ post ^. PostUserId E.==. user ^. UserId
              E.orderBy [E.desc (post ^. PostId)]
              return
                  ( post ^. PostId
                  , post ^. PostUserId
                  , post ^. PostTitle
                  , post ^. PostText
                  , user ^. UserIdent
                  )
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
