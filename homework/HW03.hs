module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state name value = (\x -> if x == name then value else state x)

empty :: State
empty = (\_ -> 0)

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var var) = state var
evalE _ (Val val) = val
evalE state (Op exp1 bop exp2)
        | bop == Plus = (+) lvalue rvalue
        | bop == Minus = (-) lvalue rvalue
        | bop == Times = (*) lvalue rvalue
        | bop == Divide = div lvalue rvalue
        | bop == Gt = if (>) lvalue rvalue then 1 else 0
        | bop == Ge = if (>=) lvalue rvalue then 1 else 0
        | bop == Lt = if (<) lvalue rvalue then 1 else 0
        | bop == Le = if (<=) lvalue rvalue then 1 else 0
        | bop == Eql = if (==) lvalue rvalue then 1 else 0
        where lvalue = evalE state exp1
              rvalue = evalE state exp2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var exp1) = DAssign var exp1
desugar (Incr var) = DAssign var (Op (Var var) Plus (Val 1))
desugar (If exp1 st1 st2) = DIf exp1 (desugar st1) (desugar st2)
desugar (While exp1 st) = DWhile exp1 (desugar st)
desugar (For st1 exp1 st2 st3) = desugar (Sequence st1 $ While exp1 $ Sequence st3 st2)
desugar (Sequence st1 st2) = DSequence (desugar st1) (desugar st2)
desugar (Skip) = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign var exp1) = extend state var $ evalE state exp1
evalSimple state (DIf exp1 diet_st1 diet_st2) = if (==) 1 $ evalE state exp1 then evalSimple state diet_st1 else evalSimple state diet_st2
evalSimple state (DWhile exp1 diet_st1) = evalSimple state $ DIf exp1 (DSequence diet_st1 $ DWhile exp1 diet_st1) DSkip
evalSimple state (DSequence diet_st1 diet_st2) = flip evalSimple diet_st2 $ evalSimple state diet_st1
evalSimple state (DSkip) = state

run :: State -> Statement -> State
run state st = evalSimple state $ desugar st

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
