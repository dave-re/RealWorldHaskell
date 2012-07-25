module MovieReview () where
	
import Control.Monad

data MovieReview = MovieReview {
	revTitle :: String,
	revUser :: String,
	revReview :: String
} deriving (Show)

apReview :: [(String, Maybe String)] -> Maybe MovieReview
apReview alist = 
	MovieReview `liftM` lookup1 "title" alist
					`ap` lookup1 "user" alist
					`ap` lookup1 "review" alist
	
lookup1 :: String -> [(String, Maybe String)] -> Maybe String
lookup1 key alist = case lookup key alist of
						Just (Just s@(_:_)) -> Just s
						_ -> Nothing