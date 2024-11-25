data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord) 
 
data Formula = Atom Var 
               |Neg Formula 
               |Formula :&: Formula 
               |Formula :|: Formula 
               |Formula :=>: Formula 
               |Formula :<=>: Formula deriving (Show, Eq, Ord) 
 
infixl 9 :&: 
infixl 9 :|: 
infixl 7 :=>: 
infixl 8 :<=>:

//1.Variables de una formula
variables :: Formula -> [String]variables (Atom p) = [p]
variables (Neg f) = variables fvariables (f1 :&: f2) = nub (variables f1 ++ variables f2)
variables (f1 :|: f2) = nub (variables f1 ++ variables f2)variables (f1 :=>: f2) = nub (variables f1 ++ variables f2)
variables (f1 :<=>: f2) = nub (variables f1 ++ variables f2)

//2. Negacion de la formula 
negacion :: Formula -> Formulanegacion (Atom p) = Neg (Atom p)
negacion (Neg f) = fnegacion (f1 :&: f2) = Neg f1 :|: Neg f2
negacion (f1 :|: f2) = Neg f1 :&: Neg f2negacion f = Neg f

//3. Equivalencia logica 
equivalencia :: Formula -> Formulaequivalencia (Atom p) = Atom p
equivalencia (Neg (Neg f)) = equivalencia fequivalencia (Neg (f1 :&: f2)) = equivalencia (Neg f1 :|: Neg f2)
equivalencia (Neg (f1 :|: f2)) = equivalencia (Neg f1 :&: Neg f2)equivalencia (Neg f) = Neg (equivalencia f)
equivalencia (f1 :&: f2) = equivalencia f1 :&: equivalencia f2equivalencia (f1 :|: f2) = equivalencia f1 :|: equivalencia f2
equivalencia (f1 :=>: f2) = equivalencia (Neg f1 :|: f2)equivalencia (f1 :<=>: f2) = equivalencia ((f1 :=>: f2) :&: (f2 :=>: f1))

//4. Interpretacion de una formula
interpretacion :: Formula -> [(String, Bool)] -> Boolinterpretacion (Atom p) vals = case lookup p vals of
  Just v -> v  Nothing -> error "No todas las variables están definidas"
interpretacion (Neg f) vals = not (in