
{- does not typecheck. Person has no Show instance
data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-}

{- Does not typecheck. No Eq instance provided nor derived

data Mood = Blah | Woot deriving Show
settleDown x = if x == Woot then Blah else x

-}

--typechecks
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
