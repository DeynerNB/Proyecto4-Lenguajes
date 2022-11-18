module CodigoHaskell.Vars (
    vars
)
where

import CodigoHaskell.Sintax

-- nub obtiene una lista sin duplicados a partir de una lista arbitraria
nub [] = []
nub (x : xs) = x : (nub (filter(\y -> x /= y ) xs))

-- Funcion
vars proposicion = do
    nub (las_vars proposicion)
    where
        las_vars prop =
            case prop of
                Constante  _             -> []
                Variable   var           -> [var]
                Negacion   prop          -> las_vars prop
                
                Conjuncion prop1 prop2   -> vars1 ++ vars2
                    where
                        vars1 = las_vars prop1
                        vars2 = las_vars prop2
                
                Disyuncion prop1 prop2   -> vars1 ++ vars2
                    where
                        vars1 = las_vars prop1
                        vars2 = las_vars prop2
                
                Implicacion prop1 prop2  -> vars1 ++ vars2
                    where
                        vars1 = las_vars prop1
                        vars2 = las_vars prop2
                
                Equivalencia prop1 prop2 -> vars1 ++ vars2
                    where
                        vars1 = las_vars prop1
                        vars2 = las_vars prop2