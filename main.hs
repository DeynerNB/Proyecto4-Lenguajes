import CodigoHaskell.As_Vals
import CodigoHaskell.EvalProp
import CodigoHaskell.Gen_Bools
import CodigoHaskell.Tabla_Verdad
import CodigoHaskell.Taut
import CodigoHaskell.Vars

import CodigoHaskell.Sintax
import FNC
import Simpl
import FND

f = Constante False
t = Constante True

p = f
q = t

vp = Variable "p"
vq = Variable "q"
vr = Variable "r"
vs = Variable "s"

-- Pruebas para FNC del PDF de Murillo (Formas Normales)
prueba1 = (Negacion(vp) .||. vq) .->. (Negacion(vq) .&&. vp)
prueba2 = (vp .->. (vq .||. vr)) .<->. (Negacion(vp) .&&. vr)

-- Respuestas del PDF esperadas para los FNC
res1 = (Negacion(vp) .||. Negacion(vq)) .&&. (vp .||. Negacion(vq)) .&&. (vp .||. vq)
res2 = (Negacion(vp) .||. Negacion(vq) .||. Negacion(vr)) .&&. (Negacion(vp) .||. Negacion(vq) .||. vr) .&&. (Negacion(vp) .||. vq .||. Negacion(vr)) .&&. (vp .||. Negacion(vq) .||. vr) .&&. (vp .||. vq .||. vr)

-- **********************************************
-- *   Para probar el resultado FNC             *
-- **********************************************

-- Ejecutar Pruebas: (imprimirProp_Estilo.generarFNC) (<variable_prueba>)

-- **********************************************
-- Pruebas de las reglas de simplificacion      *
-- **********************************************

-- ejecutarlo: (imprimirProp_Estilo.separacion) (<pruebaRegla>)

{-
    pruebaReglas_45 No la hace como se espera, esta siguiendo este proceso:

    P => (Q => R)   : 1.Implicacion y disyuncion de Q => R
    P => ~Q || R    : 1.Implicacion y disyuncion de P => (~Q V R)
    ~P || ~Q || R   : Resultado final
-}
pruebaReglas_01 = vp .->. vq
pruebaReglas_02 = Negacion (Negacion vp)
pruebaReglas_03 = Negacion (vp .||. vq)
pruebaReglas_04 = Negacion (vp .&&. vq)
pruebaReglas_05 = (vp .||. vq) .&&. (vp .||. vr)
pruebaReglas_06 = (vp .||. vq) .&&. (vr .||. vp)
pruebaReglas_07 = (vq .||. vp) .&&. (vp .||. vr)
pruebaReglas_08 = (vq .||. vp) .&&. (vr .||. vp)
pruebaReglas_09 = (vp .&&. vq) .||. (vp .&&. vr)
pruebaReglas_10 = (vp .&&. vq) .||. (vr .&&. vp)
pruebaReglas_11 = (vq .&&. vp) .||. (vp .&&. vr)
pruebaReglas_12 = (vq .&&. vp) .||. (vr .&&. vp)
pruebaReglas_13 = vp .&&. vp
pruebaReglas_14 = vp .||. vp
pruebaReglas_15 = vp .||. f
pruebaReglas_16 = f .||. vp
pruebaReglas_17 = vp .&&. t
pruebaReglas_18 = t .&&. vp
pruebaReglas_19 = Negacion f
pruebaReglas_20 = Negacion t
pruebaReglas_21 = vp .||. (Negacion vp)
pruebaReglas_22 = (Negacion vp) .||. vp
pruebaReglas_23 = vp .&&. (Negacion vp)
pruebaReglas_24 = (Negacion vp) .&&. vp
pruebaReglas_25 = vp .&&. f
pruebaReglas_26 = f .&&. vp
pruebaReglas_27 = vp .||. t
pruebaReglas_28 = t .||. vp
pruebaReglas_29 = vp .||. (vp .&&. vq)
pruebaReglas_30 = (vp .&&. vq) .||. vp
pruebaReglas_31 = vp .||. (vq .&&. vp)
pruebaReglas_32 = (vq .&&. vp) .||. vp
pruebaReglas_33 = vp .&&. (vp .||. vq)
pruebaReglas_34 = (vp .||. vq) .&&. vp
pruebaReglas_35 = vp .&&. (vq .||. vp)
pruebaReglas_36 = (vq .||. vp) .&&. vp
pruebaReglas_37 = vp .||. ((Negacion vp) .&&. vq)
pruebaReglas_38 = vp .||. (vq .&&. (Negacion vp))
pruebaReglas_39 = ((Negacion vp) .&&. vq) .||. vp
pruebaReglas_40 = (vq .&&. (Negacion vp)) .||. vp
pruebaReglas_41 = vp .&&. ((Negacion vp) .||. vq)
pruebaReglas_42 = vp .&&. (vq .||. (Negacion vp))
pruebaReglas_43 = ((Negacion vp) .||. vq) .&&. vp
pruebaReglas_44 = (vq .||. (Negacion vp)) .&&. vp
pruebaReglas_45 = vp .->. (vq .->. vr)

listaPruebas = [
    pruebaReglas_01,
    pruebaReglas_02,
    pruebaReglas_03,
    pruebaReglas_04,
    pruebaReglas_05,
    pruebaReglas_06,
    pruebaReglas_07,
    pruebaReglas_08,
    pruebaReglas_09,
    pruebaReglas_10,
    pruebaReglas_11,
    pruebaReglas_12,
    pruebaReglas_13,
    pruebaReglas_14,
    pruebaReglas_15,
    pruebaReglas_16,
    pruebaReglas_17,
    pruebaReglas_18,
    pruebaReglas_19,
    pruebaReglas_20,
    pruebaReglas_21,
    pruebaReglas_22,
    pruebaReglas_23,
    pruebaReglas_24,
    pruebaReglas_25,
    pruebaReglas_26,
    pruebaReglas_27,
    pruebaReglas_28,
    pruebaReglas_29,
    pruebaReglas_30,
    pruebaReglas_31,
    pruebaReglas_32,
    pruebaReglas_33,
    pruebaReglas_34,
    pruebaReglas_35,
    pruebaReglas_36,
    pruebaReglas_37,
    pruebaReglas_38,
    pruebaReglas_39,
    pruebaReglas_40,
    pruebaReglas_41,
    pruebaReglas_42,
    pruebaReglas_43,
    pruebaReglas_44,
    pruebaReglas_45]


printLista numero [] = return ()
printLista numero (x:xs) = do
    let numeroSumado = numero+1
    putStrLn ("Prueba #" ++ (show numero))
    
    putStr ((imprimirProp_Estilo x) ++ " -> Simplificacion -> ")

    putStrLn (imprimirProp_Estilo (simplificacion x))

    putStr "\n"
    
    printLista numeroSumado xs