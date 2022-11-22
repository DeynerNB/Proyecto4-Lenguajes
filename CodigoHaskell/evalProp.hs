module CodigoHaskell.EvalProp (
    evalProp,
    Ambiente,
    Identificador
)
where

import CodigoHaskell.Sintax

type Identificador = String
type Ambiente = [(Identificador, Bool)]

-- Busqueda en el ambiente
-- Identificador no esta definido en el ambiente, se levanta una excepcion

busca ident []                           = error "No esta en el dominio"
busca ident ((ident1, valor) : ambiente) = do
    if ident1 == ident then
        valor
    else
        busca ident ambiente

evalProp ambiente prop =
    case prop of
        Constante  val           -> val
        Variable   var           -> busca var ambiente
        Negacion   prop          -> not(evalProp ambiente prop)
        
        Conjuncion prop1 prop2   -> valor1 && valor2
            where
                valor1 = evalProp ambiente prop1
                valor2 = evalProp ambiente prop2
        
        Disyuncion prop1 prop2   -> valor1 || valor2
            where
                valor1 = evalProp ambiente prop1
                valor2 = evalProp ambiente prop2
        
        Implicacion prop1 prop2  -> not(valor1) || valor2
            where
                valor1 = evalProp ambiente prop1
                valor2 = evalProp ambiente prop2
        
        Equivalencia prop1 prop2 -> valor1 == valor2
            where
                valor1 = evalProp ambiente prop1
                valor2 = evalProp ambiente prop2
        


