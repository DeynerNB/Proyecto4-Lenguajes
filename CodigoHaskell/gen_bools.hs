
module CodigoHaskell.Gen_Bools (
    gen_bools
)
where

-- Construir una lista
construirLista x xs = x : xs

-- Genera una lista que contiene listas de n variables, cada lista es una
-- combinacion para las n variables disponibles
gen_bools 0 = [[]]
gen_bools n = do
        (map (construirLista True) anterior) ++ (map (construirLista False) anterior)
    where
        anterior = gen_bools (n - 1)