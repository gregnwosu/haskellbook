module Construction where

data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b} deriving (Eq,Show)


newtype NumCow = NumCow Int
                 deriving (Eq,Show)

newtype NumPig = NumPig Int
                 deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig
                 deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int
                   deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep
                    deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)



type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age
               deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud
               deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool
                 deriving (Eq, Show)

data Animal = Cow CowInfo  | Pig PigInfo | Sheep SheepInfo
              deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

type Awesome = Bool
type Name = String

person:: Product Name Awesome
person = Product "Simon" True


data Twitter = Twitter deriving (Eq, Show )

data AskFm = AskFm deriving (Eq, Show)


socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter


data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows
                       deriving (Eq, Show)

data ProgrammingLanguage = Haskell | Agda | Idris | Purescript
                           deriving (Eq , Show)


data Programmer = Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage}
                  deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac , Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, Purescript]

allProgrammers :: [Programmer]
allProgrammers = do
    os' <- allOperatingSystems
    l' <- allLanguages
    return  Programmer {os = os', lang = l'}

isCorrect = length allProgrammers == length allOperatingSystems * (length allLanguages)

data ThereYet = There Integer Float String Bool
                deriving (Eq, Show)
