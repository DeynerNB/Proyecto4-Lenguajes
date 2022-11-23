module FND (
    generarFND
)
where

import CodigoHaskell.Sintax
import CodigoHaskell.Tabla_Verdad

procesoFnd []       = []
procesoFnd (x : xs) = do
        generarConjunciones x ++ (procesoFnd xs)
    where

        -- Genera conjunciones de las variables
        -- Si el valor de una variable es False -> Aplica negacion a la variable
        negarFalsas (x : xs) = do
                if (xs == []) then
                    esFalsa x
                else
                    Conjuncion (esFalsa x) (negarFalsas xs)
            where
                esFalsa(var, valor) = if valor then Variable(var) else Negacion(Variable(var))

        -- Utiliza las proposiciones cuyo resultado fue VERDADERO para agregar a la lista de conjunciones
        generarConjunciones (listaVariables, evaluacion) = do
                if evaluacion then
                    [negarFalsas listaVariables]
                else
                    []

generarFND prop = do
        generarDisyunciones listaConjunciones
    where
        listaConjunciones = procesoFnd (tabla_dato prop)
        generarDisyunciones (x : xs) = if (length xs == 0) then x else Disyuncion x (generarDisyunciones xs)
