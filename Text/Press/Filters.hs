module Text.Press.Filters
    ( defaultFilters )
    where

import Debug.Trace
import Data.Map (Map, lookup, fromList, insert)
import Data.Char (toUpper, toLower)
import Data.List (intersperse)

import Text.JSON.Types 
import Text.JSON
    
import Text.Press.Types

noArg :: (JSValue -> JSValue) -> FilterFunc
noArg f = \_ -> f

-- | A helper, that takes a function of @String->String@ and makes
-- it operate of a 'JSString'. 
applyJSString :: (String -> String) -> JSValue -> JSValue
applyJSString f (JSString x) = JSString . toJSString . f . fromJSString $ x
applyJSString _ x = x

-- | Return the first element of an array
firstFilt :: JSValue -> JSValue
firstFilt (JSArray xs) | null xs = JSNull
                       | otherwise = head xs
firstFilt x = x

-- | Return the last element of an array
lastFilt :: JSValue -> JSValue
lastFilt (JSArray xs) | null xs = JSNull
                      | otherwise = last xs
lastFilt x = x

-- | Return the length of a string, or an array
lengthFilt :: JSValue -> JSValue
lengthFilt s@(JSString _) = applyJSString (show . length) s
lengthFilt (JSArray xs) = JSString . toJSString . show . length $ xs
lengthFilt x = x

-- | Remove characters from the string, eg. ig x="abcdef"
-- then {{ x|cut:"bf" }}  will produce "acde"
cut :: [JSValue] -> JSValue -> JSValue
cut ((JSString c):_) = applyJSString $ filter (`notElem` (fromJSString c))
cut _ = id

-- | Convert the elements of the array to Strings, then join
-- the string with the given argument between each element.
-- No argument just concats the elements together
join :: [JSValue] -> JSValue -> JSValue
join (arg:_) (JSArray array) = 
    let c = toStr arg
        toStr (JSString s) = fromJSString s
        toStr v = showJSValue v ""
    in JSString . toJSString . concat . intersperse c $ map toStr array 
join _ x = x
    
cycleFilt :: [JSValue] -> JSValue -> JSValue
cycleFilt args (JSRational _ rat) = let n = floor rat
                                    in args!!(n `mod` (length args))
cycleFilt args p = JSNull

defaultFilters :: Map String FilterFunc
defaultFilters = fromList 
                   [("upper", noArg $ applyJSString (map toUpper))
                   ,("lower", noArg $ applyJSString (map toLower))
                   ,("first", noArg firstFilt)
                   ,("last",  noArg lastFilt)
                   ,("length", noArg lengthFilt)
                   ,("cut", cut)
                   ,("join", join)
                   ,("cycle", cycleFilt)
                   ]
