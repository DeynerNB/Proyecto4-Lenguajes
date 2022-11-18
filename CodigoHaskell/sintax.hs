-- Tipo de dato de proposicion

module CodigoHaskell.Sintax (
    Proposicion(
        Constante,
        Variable,
        Negacion,
        Conjuncion,
        Disyuncion,
        Implicacion,
        Equivalencia
    ),
    (.&&.),
    (.||.),
    (.->.),
    (.<->.),
    imprimirProp,
    imprimirProp_Estilo
)
where

data Proposicion =
    Constante      Bool
    | Variable     String
    | Negacion     Proposicion
    | Conjuncion   Proposicion Proposicion
    | Disyuncion   Proposicion Proposicion
    | Implicacion  Proposicion Proposicion
    | Equivalencia Proposicion Proposicion


infixl 7 .&&.
(.&&.) :: Proposicion -> Proposicion -> Proposicion
x .&&. y = Conjuncion x y

infixl 6 .||.
(.||.) :: Proposicion -> Proposicion -> Proposicion
x .||. y = Disyuncion x y

infixr 5 .->.
(.->.) :: Proposicion -> Proposicion -> Proposicion
x .->. y = Implicacion x y

infixl 4 .<->.
(.<->.) :: Proposicion -> Proposicion -> Proposicion
x .<->. y = Equivalencia x y

imprimirProp prop = 
    case prop of
        Constante True           -> "true"
        Constante False          -> "false"
        Variable var             -> var
        Negacion prop1           -> "negacion (" ++ imprimirProp  prop1 ++ ")"
        Conjuncion prop1 prop2   -> "conjuncion (" ++ imprimirProp prop1 ++ ", " ++ imprimirProp prop2 ++ ")"
        Disyuncion prop1 prop2   -> "disyuncion (" ++ imprimirProp prop1 ++ ", " ++ imprimirProp prop2 ++ ")"
        Implicacion prop1 prop2  -> "implicacion (" ++ imprimirProp prop1 ++ ", " ++ imprimirProp prop2 ++ ")"
        Equivalencia prop1 prop2 -> "equivalencia (" ++ imprimirProp prop1 ++ ", " ++ imprimirProp prop2 ++ ")"

imprimirProp_Estilo prop = 
    case prop of
        Constante True           -> "true"
        Constante False          -> "false"
        Variable var             -> var
        Negacion prop1           -> " ~" ++ imprimirProp_Estilo prop1 ++ " "
        Conjuncion prop1 prop2   -> " " ++ imprimirProp_Estilo prop1 ++ " && " ++ imprimirProp_Estilo prop2 ++ " "
        Disyuncion prop1 prop2   -> " " ++ imprimirProp_Estilo prop1 ++ " || " ++ imprimirProp_Estilo prop2 ++ " "
        Implicacion prop1 prop2  -> " " ++ imprimirProp_Estilo prop1 ++ " => " ++ imprimirProp_Estilo prop2 ++ " "
        Equivalencia prop1 prop2 -> " " ++ imprimirProp_Estilo prop1 ++ " <=> " ++ imprimirProp_Estilo prop2 ++ " "
