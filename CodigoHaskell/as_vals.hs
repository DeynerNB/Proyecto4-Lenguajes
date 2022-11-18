module CodigoHaskell.As_Vals (
    as_vals,
    impr_as_vals
)
where

-- Zip: Combinar dos listas y producir una lista de pares (x, y)
zipE []        []        = []
zipE (x : xs) (y : ys)   = (x, y) : zipE xs ys
zipE _         _         = error "Listas de longitud diferentes"

-- Hacer uso de Zip con las variables y los valores booleanos
as_vals vars bools = zipE vars bools

-- Mostrar los valores
impr_as_vals []             = ""
impr_as_vals ((x, y) : xys) = "(" ++ x ++ "," ++ (if y then "true" else "false") ++ ") " ++ (impr_as_vals xys)

