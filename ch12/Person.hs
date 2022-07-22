module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data InvalidPerson = NameEmpty | AgeTooLow deriving (Eq, Show)
type ValidatePerson a = Either [InvalidPerson] a

nameOkay :: Name -> ValidatePerson Name
nameOkay "" = Left [NameEmpty]
nameOkay name = Right name

ageOkay :: Age -> ValidatePerson Age
ageOkay age = if age >= 0 
  then Right age
  else Left [AgeTooLow]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Left badName) (Left badAge) = Left $ badName ++ badAge
mkPerson' (Left badName) (Right _) = Left badName
mkPerson' (Right _) (Left badAge) = Left badAge
mkPerson' (Right name) (Right age) = Right $ Person name age
