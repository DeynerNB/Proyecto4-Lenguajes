
module FNC (
    generarFNC
)
where

import CodigoHaskell.Sintax
import CodigoHaskell.Tabla_Verdad
import CodigoHaskell.Vars
import CodigoHaskell.EvalProp

procesoFnc []       = []
procesoFnc (x : xs) = do
        generarDisyunciones x ++ (procesoFnc xs)
    where

        -- Genera conjunciones de las variables
        -- Si el valor de una variable es TRUE -> Aplica negacion a la variable
        negarVerdaderas (x : xs) = do
                if (xs == []) then
                    esVerdadera x
                else
                    Disyuncion (esVerdadera x) (negarVerdaderas xs)
            where
                esVerdadera(var, valor) = if valor then Negacion(Variable(var)) else Variable(var)

        -- Utiliza las proposiciones cuyo resultado fue FALSO para formar parte lista de conjunciones
        generarDisyunciones (listaVariables, evaluacion) = do
                if not(evaluacion) then
                    [negarVerdaderas listaVariables]
                else
                    []

generarFNC prop = do
        generarConjunciones listaConjunciones
    where
        listaConjunciones = procesoFnc (tabla_dato prop)
        generarConjunciones (x : xs) = do
            -- Verificar si es una proposición de solo constantes
            if ((length (vars prop)) == 0) then 
                if ((evalProp [] prop) == True) then Constante True 
                else Constante False
            else
                if (length xs == 0) then x else Conjuncion x (generarConjunciones xs)
