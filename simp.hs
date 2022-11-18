
module Simp (
    separacion
)
where

import CodigoHaskell.Sintax

instance Eq Proposicion where
    Constante x         == Constante y          = x == y
    Variable x          == Variable y           = x == y
    Negacion x          == Negacion y           = x == y
    Conjuncion x1 y1    == Conjuncion x2 y2     = x1 == x2 && y1 == y2
    Disyuncion x1 y1    == Disyuncion x2 y2     = x1 == x2 && y1 == y2
    Implicacion x1 y1   == Implicacion x2 y2    = x1 == x2 && y1 == y2
    Equivalencia x1 y1  == Equivalencia x2 y2   = x1 == x2 && y1 == y2
    _                   == _                    = False

simplificacion prop =
    case prop of
        -- Doble Negacion y Morgan
        Negacion (Negacion p1) -> p1
        Negacion (Disyuncion p1 p2) -> Conjuncion (Negacion p1) (Negacion p2)
        Negacion (Conjuncion p1 p2) -> Disyuncion (Negacion p1) (Negacion p2)
        -- Distributiva
        Conjuncion (Disyuncion p1 p2) (Disyuncion p3 p4) ->
            if (p1 == p3) then
                Disyuncion p1 (Conjuncion p2 p4)
            else if (p1 == p4) then
                Disyuncion p1 (Conjuncion p2 p3)
            else if (p2 == p3) then
                Disyuncion p2 (Conjuncion p1 p4)
            else if (p2 == p4) then
                Disyuncion p2 (Conjuncion p1 p3)
            else
                prop
        Disyuncion (Conjuncion p1 p2) (Conjuncion p3 p4) ->
            if (p1 == p3) then
                Conjuncion p1 (Disyuncion p2 p4)
            else if (p1 == p4) then
                Conjuncion p1 (Disyuncion p2 p3)
            else if (p2 == p3) then
                Conjuncion p2 (Disyuncion p1 p4)
            else if (p2 == p4) then
                Conjuncion p2 (Disyuncion p1 p3)
            else
                prop
        -- Neutro
        Disyuncion p1 (Constante False) -> p1
        Disyuncion (Constante False) p1 -> p1
        Conjuncion p1 (Constante True) -> p1
        Conjuncion (Constante True) p1 -> p1
        -- Inverso
        Negacion (Constante False)  -> Constante True
        Negacion (Constante True)   -> Constante False
        Disyuncion p1 (Negacion p2) -> if (p1 == p2) then Constante True else prop
        Disyuncion (Negacion p2) p1 -> if (p1 == p2) then Constante True else prop
        Conjuncion p1 (Negacion p2) -> if (p1 == p2) then Constante False else prop
        Conjuncion (Negacion p2) p1 -> if (p1 == p2) then Constante False else prop
        -- Dominacion
        Conjuncion p1 (Constante False) -> Constante False
        Conjuncion (Constante False) p1 -> Constante False
        Disyuncion p1 (Constante True)  -> Constante True
        Disyuncion (Constante True) p1  -> Constante True
        -- Absorcion v2
        Disyuncion p1 (Conjuncion (Negacion p2) p3) -> if (p1 == p2) then Disyuncion p1 p3 else prop
        Disyuncion p1 (Conjuncion p3 (Negacion p2)) -> if (p1 == p2) then Disyuncion p1 p3 else prop
        Disyuncion (Conjuncion (Negacion p2) p3) p1 -> if (p1 == p2) then Disyuncion p1 p3 else prop
        Disyuncion (Conjuncion p3 (Negacion p2)) p1 -> if (p1 == p2) then Disyuncion p1 p3 else prop
        Conjuncion p1 (Disyuncion (Negacion p2) p3) -> if (p1 == p2) then Conjuncion p1 p3 else prop
        Conjuncion p1 (Disyuncion p3 (Negacion p2)) -> if (p1 == p2) then Conjuncion p1 p3 else prop
        Conjuncion (Disyuncion (Negacion p2) p3) p1 -> if (p1 == p2) then Conjuncion p1 p3 else prop
        Conjuncion (Disyuncion p3 (Negacion p2)) p1 -> if (p1 == p2) then Conjuncion p1 p3 else prop
        -- Absorcion v1
        Disyuncion p1 (Conjuncion p2 p3) -> if (p1 == p2 || p1 == p3) then p1 else prop
        Disyuncion (Conjuncion p2 p3) p1 -> if (p1 == p2 || p1 == p3) then p1 else prop
        Conjuncion p1 (Disyuncion p2 p3) -> if (p1 == p2 || p1 == p3) then p1 else prop
        Conjuncion (Disyuncion p2 p3) p1 -> if (p1 == p2 || p1 == p3) then p1 else prop
        -- Exportacion
        Implicacion p1 (Implicacion p2 p3) -> Implicacion (Conjuncion p1 p2) p3
        -- Implicacion y disyuncion
        Implicacion p1 p2 -> Disyuncion (Negacion p1) p2
        -- Idempotencia
        Conjuncion p1 p2 -> if (p1 == p2) then p1 else prop
        Disyuncion p1 p2 -> if (p1 == p2) then p1 else prop
        _                -> prop

separacion prop =
    case prop of
        Constante True  -> Constante True
        Constante False -> Constante False
        Variable var    -> Variable var
        
        Negacion p1 -> do
                if res == prop then res else (separacion res)
            where
                valor = separacion p1
                res = simplificacion (Negacion valor)
        
        Conjuncion p1 p2 -> do
                if res == prop then res else (separacion res)
            where
                valor1 = separacion p1
                valor2 = separacion p2
                res = simplificacion (Conjuncion valor1 valor2)
        
        Disyuncion p1 p2 -> do
                if res == prop then res else (separacion res)
            where
                valor1 = separacion p1
                valor2 = separacion p2
                res = simplificacion (Disyuncion valor1 valor2)
        
        Implicacion p1 p2 -> do
                if res == prop then res else (separacion res)
            where
                valor1 = separacion p1
                valor2 = separacion p2
                res = simplificacion (Implicacion valor1 valor2)
        
        Equivalencia p1 p2 -> do
                if res == prop then res else (separacion res)
            where
                valor1 = separacion p1
                valor2 = separacion p2
                res = simplificacion (Equivalencia valor1 valor2)

