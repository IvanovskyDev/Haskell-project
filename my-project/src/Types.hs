module Types where


-- нетерминал, "S", "AB"
newtype State = State {unState :: String}
    deriving(Eq,Ord,Show)

-- терминал
data Terminal = Term Char | Eps 
    deriving(Eq,Ord,Show)

-- переход
data Transition = Transition {
    from  :: State,
    onTerm:: Terminal,
    to    :: State
    } deriving(Eq,Ord,Show)


data NKA = NKA{
    nkaStates     :: [State],
    nkaTerminals  :: [Char],
    nkaTransitions:: [(State,Terminal,State)],
    nkaStart      :: State,
    nkaFinal      :: [State]
    } deriving(Eq,Show)


data DKA = DKA{
    dkaStates     :: [State],
    dkaTerminals  :: [Char],
    dkaTransitions:: [((State,Char),State)], -- типо [ключ, значение]
    dkaStart      :: State,
    dkaFinal      :: [State]
    } deriving(Eq,Show)


data Rules = Terminal Char
           | RightGrammar Char State
           | LeftGrammar State Char
           | NonTerminal State
           | Epsilon
            deriving(Eq,Show)


data Grammar = Grammar{
    grammarNonTerminals:: [State],
    grammarTerminals   :: [Char],
    grammarStart       :: State,
    grammarRules       :: [(State,[Rules])],
    grammarLeft        :: Bool
    } deriving(Eq,Show)