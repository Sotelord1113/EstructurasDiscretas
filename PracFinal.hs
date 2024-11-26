import Data.List (nub)

-- Definición de tipos
data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)
data Formula = Atom Var
            | Neg Formula
            | Formula :&: Formula
            | Formula :|: Formula
            | Formula :=>: Formula
            | Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-- 1. Variables de una fórmula
variables :: Formula -> [Var]
variables (Atom v) = [v]
variables (Neg f) = variables f
variables (f1 :&: f2) = nub (variables f1 ++ variables f2)
variables (f1 :|: f2) = nub (variables f1 ++ variables f2)
variables (f1 :=>: f2) = nub (variables f1 ++ variables f2)
variables (f1 :<=>: f2) = nub (variables f1 ++ variables f2)

-- 2. Negación de una fórmula
negacion :: Formula -> Formula
negacion (Atom v) = Neg (Atom v)
negacion (Neg f) = f
negacion (f1 :&: f2) = Neg f1 :|: Neg f2
negacion (f1 :|: f2) = Neg f1 :&: Neg f2
negacion (f1 :=>: f2) = f1 :&: Neg f2
negacion (f1 :<=>: f2) = Neg (f1 :=>: f2) :|: Neg (f2 :=>: f1)

-- 3. Equivalencia de una fórmula
equivalencia :: Formula -> Formula
equivalencia (Atom v) = Atom v
equivalencia (Neg (Atom v)) = Neg (Atom v)
equivalencia (Neg f) = Neg (equivalencia f)
equivalencia (f1 :&: f2) = equivalencia f1 :&: equivalencia f2
equivalencia (f1 :|: f2) = equivalencia f1 :|: equivalencia f2
equivalencia (f1 :=>: f2) = Neg (equivalencia f1) :|: equivalencia f2
equivalencia (f1 :<=>: f2) = (Neg (equivalencia f1) :|: equivalencia f2) :&: (Neg (equivalencia f2) :|: equivalencia f1)

-- 4. Interpretación de una fórmula
interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom v) valores = if esVariableDefinida valores v then obtenerValor valores v else error "No todas las variables están definidas"
interpretacion (Neg f) valores = not (interpretacion f valores)
interpretacion (f1 :&: f2) valores = interpretacion f1 valores && interpretacion f2 valores
interpretacion (f1 :|: f2) valores = interpretacion f1 valores || interpretacion f2 valores
interpretacion (f1 :=>: f2) valores = not (interpretacion f1 valores) || interpretacion f2 valores
interpretacion (f1 :<=>: f2) valores = interpretacion f1 valores == interpretacion f2 valores

esVariableDefinida :: [(Var, Bool)] -> Var -> Bool
esVariableDefinida [] _ = False
esVariableDefinida ((x, _):xs) v = x == v || esVariableDefinida xs v

obtenerValor :: [(Var, Bool)] -> Var -> Bool
obtenerValor ((x, b):xs) v = if x == v then b else obtenerValor xs v

interpretacionAux :: Formula -> [(Var, Bool)] -> Bool
interpretacionAux (Atom v) valores = if null [x | (x, _) <- valores, x == v] then error "No todas las variables están definidas" else head [b | (x, b) <- valores, x == v]
interpretacionAux (Neg f) valores = not (interpretacionAux f valores)
interpretacionAux (f1 :&: f2) valores = interpretacionAux f1 valores && interpretacionAux f2 valores
interpretacionAux (f1 :|: f2) valores = interpretacionAux f1 valores || interpretacionAux f2 valores
interpretacionAux (f1 :=>: f2) valores = not (interpretacionAux f1 valores) || interpretacionAux f2 valores
interpretacionAux (f1 :<=>: f2) valores = interpretacionAux f1 valores == interpretacionAux f2 valores

-- 5. Combinaciones de una fórmula
combinaciones :: Formula -> [[(Var, Bool)]]
combinaciones f = map (crearAsociaciones (variables f)) (secuencias (length (variables f)))

crearAsociaciones :: [Var] -> [Bool] -> [(Var, Bool)]
crearAsociaciones [] [] = []
crearAsociaciones (v:vs) (b:bs) = (v, b) : crearAsociaciones vs bs

secuencias :: Int -> [[Bool]]
secuencias 0 = [[]]
secuencias n = [False : xs | xs <- secuencias (n-1)] ++ [True : xs | xs <- secuencias (n-1)]

-- 6. Tabla de verdad de una fórmula
tablaDeVerdad :: Formula -> [([(Var, Bool)], Bool)]
tablaDeVerdad f = [(c, interpretacion f c) | c <- combinaciones f]