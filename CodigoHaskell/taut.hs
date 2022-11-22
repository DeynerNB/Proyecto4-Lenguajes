
module CodigoHaskell.Taut (
    taut
)
where

-- Determinar si una proposición es una tautología
import CodigoHaskell.As_Vals
import CodigoHaskell.EvalProp
import CodigoHaskell.Gen_Bools
import CodigoHaskell.Vars

taut prop = do
        if recorrer lista_combinaciones_booleanas then
            print("Es tautologia")
        else
            print("No es tautologia")
    where
        variables = vars prop
        n = length variables
        lista_combinaciones_booleanas = gen_bools n

        recorrer [] = True
        recorrer (fila : filas) = do
                if evaluacion_es_verdadera then
                    recorrer filas
                else
                    False
            where
                asociacion = as_vals variables fila
                evaluacion_es_verdadera = evalProp asociacion prop


