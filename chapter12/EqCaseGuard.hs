module EqCaseGuard where

data PersonInvalid = NameEmpty | AgeTooLow

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

instance Eq PersonInvalid where
  (==) = (==)

blah:: PersonInvalid -> String
blah pi
 | pi == NameEmpty = "NameEmpty"
 | pi == AgeTooLow = "AgeTooLow"
 | otherwise  = "???"

mkPerson1 :: Name -> Age -> Either PersonInvalid Person
mkPerson1 name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

ageOkay:: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [NameEmpty]
nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]
mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)
mkPerson' ::
  ValidatePerson Name ->
  ValidatePerson Age ->
  ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) =
  Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) =
  Left (badName ++ badAge)
mkPerson' (Left badName) _ =
  Left badName
mkPerson' _ (Left badAge) =
  Left badAge
