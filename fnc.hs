
module FNC (
    generarFNC
)
where

import CodigoHaskell.Sintax
import CodigoHaskell.Tabla_Verdad

procesoFnc []       = []
procesoFnc (x : xs) = do
        generarConjunciones x ++ (procesoFnc xs)
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
        generarConjunciones (listaVariables, evaluacion) = do
                if not(evaluacion) then
                    [negarVerdaderas listaVariables]
                else
                    []

generarFNC prop = do
        generarDisyunciones listaConjunciones
    where
        listaConjunciones = procesoFnc (tabla_dato prop)
        generarDisyunciones (x : xs) = if (length xs == 0) then x else Conjuncion x (generarDisyunciones xs)
