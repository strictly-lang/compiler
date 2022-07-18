module Emitter.Util where

import Control.Monad.State.Lazy (MonadState (state))
import Data.Char (toUpper)
import Emitter.Types
import Types

type AbsolutePath = String

type ProjectPath = String

pathToComponentName :: ProjectPath -> AbsolutePath -> Maybe String
pathToComponentName [] (a : as) = Just (removeFileExtension as)
pathToComponentName (p : ps) (a : as)
  | p == a = pathToComponentName ps as
  | otherwise = Nothing

getFreshExprId :: AppStateMonad Int
getFreshExprId =
  state
    ( \(AppState {componentName = componentName, expressionIdCounter = expressionIdCounter, modules = modules}) ->
        ( expressionIdCounter,
          AppState
            { componentName = componentName,
              expressionIdCounter = expressionIdCounter + 1,
              modules = modules
            }
        )
    )

getModule :: String -> String -> AppStateMonad String
getModule moduleName importName =
  state
    ( \(AppState {componentName = componentName, expressionIdCounter = expressionIdCounter, modules = modules}) ->
        let (expressionIdCounter', importName', modules') = addModule moduleName importName expressionIdCounter modules
         in ( importName',
              AppState
                { componentName = componentName,
                  expressionIdCounter = expressionIdCounter',
                  modules = modules'
                }
            )
    )

addModule :: String -> String -> Int -> [Module] -> (Int, String, [Module])
addModule moduleName importName expressionIdCounter [] =
  let expressionIdCounter' = expressionIdCounter + 1
      importName' = importName ++ show expressionIdCounter
   in (expressionIdCounter', importName', [(moduleName, [(importName, importName')])])

removeFileExtension :: String -> String
removeFileExtension p = take (length p - length ".sly") p

instance Show Variable where
  show (DotNotation name) = '.' : name
  show (BracketNotation name) = '[' : name ++ "]"

nameToVariable :: String -> Int -> [Variable]
nameToVariable name exprId = [DotNotation (name ++ show exprId)]

variableToString :: [Variable] -> String
variableToString ((DotNotation v) : vs) = concat (v : map show vs)

slashToDash :: String -> String
slashToDash [] = []
slashToDash ('/' : ps) = '-' : slashToDash ps
slashToDash (p : ps) = p : slashToDash ps

slashToCamelCase :: String -> String
slashToCamelCase (p : ps) = toUpper p : slashToCamelCase' ps

slashToCamelCase' :: String -> String
slashToCamelCase' [] = []
slashToCamelCase' ('/' : p : ps) = toUpper p : slashToCamelCase' ps
slashToCamelCase' (p : ps) = p : slashToCamelCase' ps

type GroupedProperty = (String, TypeDefinition, [UntypedExpression])

groupProperties :: [(String, RecordValue)] -> [GroupedProperty]
groupProperties = groupProperties' Nothing

groupProperties' :: Maybe GroupedProperty -> [(String, RecordValue)] -> [GroupedProperty]
groupProperties' Nothing [] = []
groupProperties' Nothing ((name, RecordType recordType) : nextProperties) = groupProperties' (Just (name, recordType, [])) nextProperties
groupProperties' Nothing ((name, RecordExpression Nothing untypedExpression) : nextProperties) = groupProperties' (Just (name, TypeUnknown, [untypedExpression])) nextProperties
groupProperties' (Just groupedProperty) [] = [groupedProperty]
groupProperties' (Just previous@(groupedPropertyName, groupedPropertyType, groupedPropertyExpressions)) ((name, RecordExpression Nothing untypedExpression) : nextProperties)
  | groupedPropertyName == name =
    groupProperties' (Just (name, groupedPropertyType, groupedPropertyExpressions ++ [untypedExpression])) nextProperties
  | otherwise =
    previous : groupProperties' (Just (name, TypeUnknown, [untypedExpression])) nextProperties
groupProperties' (Just previous@(groupedPropertyName, groupedPropertyType, groupedPropertyExpressions)) ((name, RecordType recordType) : nextProperties)
  | groupedPropertyName == name =
    error ("cant have to types for " ++ name)
  | otherwise =
    previous : groupProperties' (Just (name, recordType, [])) nextProperties
