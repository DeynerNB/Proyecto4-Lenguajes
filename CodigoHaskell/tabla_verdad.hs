
module CodigoHaskell.Tabla_Verdad (
    tabla,
    tabla_str,
    tabla_dato
)
where

import CodigoHaskell.As_Vals
import CodigoHaskell.EvalProp
import CodigoHaskell.Gen_Bools
import CodigoHaskell.Vars

--Generar (como dato) la tabla de verdad de una proposición con variables
tabla_dato prop = do
        recorrer lista_combinaciones_booleanas
    where
        variables = vars prop
        n = length variables
        lista_combinaciones_booleanas = gen_bools n

        recorrer []             = []
        recorrer (fila : filas) = do
                (asociacion, resultado_fila) : recorrer filas
            where
                asociacion = as_vals variables fila
                resultado_fila = evalProp asociacion prop

-- Funcion
tabla prop = do
        recorrer lista_combinaciones_booleanas
    where
        variables = vars prop
        n = length variables
        lista_combinaciones_booleanas = gen_bools n

        imprimir_fila vars_bools es_verdadero = putStrLn (impr_as_vals vars_bools ++ " === " ++ (if es_verdadero then "true" else "false"))

        recorrer []             = putStrLn ""
        recorrer (fila : filas) = do
                imprimir_fila asociacion resultado_fila
                recorrer filas
            where
                asociacion = as_vals variables fila
                resultado_fila = evalProp asociacion prop

-- Funcion
tabla_str prop = do
        recorrer lista_combinaciones_booleanas
    where
        variables = vars prop
        n = length variables
        lista_combinaciones_booleanas = gen_bools n

        imprimir_fila vars_bools es_verdadero = impr_as_vals vars_bools ++ " === " ++ (if es_verdadero then "true" else "false") ++ "\n"

        recorrer []             = ""
        recorrer (fila : filas) = do
                (imprimir_fila  asociacion  resultado_fila) ++ recorrer filas
            where
                asociacion = as_vals variables fila
                resultado_fila = evalProp asociacion prop
