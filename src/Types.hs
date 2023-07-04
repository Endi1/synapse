module Types (Command (..), Options, Arguments, mkArguments, command, options) where

data Command = Import | Serve deriving (Show)

newtype Options = Options {unOptions :: [String]} deriving (Show)

data Arguments = Arguments Command Options deriving (Show)

mkArguments :: [String] -> Either String Arguments
mkArguments (x : xs) = do
  command' <- case mkCommand x of
    Nothing -> Left $ "Command " ++ x ++ " not supported"
    Just c -> return c

  options' <- case mkOptions command' xs of
    Nothing -> Left $ "Options " ++ show xs ++ " not supported"
    Just o -> return o

  Right $ Arguments command' options'
mkArguments [] = Left "No arguments were passed"

mkCommand :: String -> Maybe Command
mkCommand "import" = Just Import
mkCommand "serve" = Just Serve
mkCommand _ = Nothing

mkOptions :: Command -> [String] -> Maybe Options
mkOptions Import potentialOptions = if length potentialOptions == 1 then Just $ Options potentialOptions else Nothing
mkOptions Serve _ = Just $ Options []

command :: Arguments -> Command
command (Arguments c _) = c

options :: Arguments -> [String]
options (Arguments _ (Options os)) = os