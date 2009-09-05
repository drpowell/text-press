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
cut :: Maybe JSValue -> JSValue -> JSValue
cut Nothing = id
cut (Just (JSString c)) = applyJSString $ filter (`notElem` (fromJSString c))

-- | Convert the elements of the array to Strings, then join
-- the string with the given argument between each element.
-- No argument just concats the elements together
join :: Maybe JSValue -> JSValue -> JSValue
join arg (JSArray array) = 
    let c = maybe "" toStr arg
        toStr (JSString s) = fromJSString s
        toStr v = showJSValue v ""
    in JSString . toJSString . concat . intersperse c $ map toStr array 
join _ x = x
    
defaultFilters :: Map String FilterFunc
defaultFilters = fromList 
                   [("upper", FilterNoArg $ applyJSString (map toUpper))
                   ,("lower", FilterNoArg $ applyJSString (map toLower))
                   ,("first", FilterNoArg firstFilt)
                   ,("last",  FilterNoArg lastFilt)
                   ,("length", FilterNoArg lengthFilt)
                   ,("cut", FilterArg cut)
                   ,("join", FilterArg join)
                   ]
