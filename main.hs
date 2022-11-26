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

-- Se pueden ejecutar pruebas: (imprimirProp_Estilo.generarFNC) (<variable_prueba>)

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

-- **********************************************
-- *   Para probar la función FNC             *
--   Para ver el resultado de la equivalencia se escribe en la consola result_FNC<#prueba>
--   #prueba = 1-3 pruebas con proposiciones sin variables
--   #prueba = 4-9 pruebas con proposiciones con variables
--   #prueba = 10-11 pruebas con proposiciones con variables y isn variables que son contradicciones
-- Se pueden ejecutar pruebas: (imprimirProp_Estilo.fnc) (<variable_prueba>)
-- **********************************************

-- *************** Proposiciones sin variables   ****************
fnc_pruebaCons1 = f .->. f .<->. (Negacion f) .||. f
fnc_pruebaCons2 = f .->. t .<->. (Negacion f) .||. t
fnc_pruebaCons3 = f .->. t .<->. (Negacion t) .->. (Negacion f)

pc1FNC = (fnc) (fnc_pruebaCons1)
pc2FNC = (fnc) (fnc_pruebaCons2)
pc3FNC = (fnc) (fnc_pruebaCons3)

fnc_test1 = fnc_pruebaCons1 .<->. pc1FNC
fnc_test2 = fnc_pruebaCons2 .<->. pc2FNC
fnc_test3 = fnc_pruebaCons3 .<->. pc3FNC

result_FNC1 = (taut) (fnc_test1)
result_FNC2 = (taut) (fnc_test2)
result_FNC3 = (taut) (fnc_test3)

-- *************** Proposiciones con variables   ****************

pruebaFNC_4 = ((vp .&&. vq) .||. ((Negacion vp) .||. vr )) .||. (vq .&&. vr)   
pruebaFNC_5 = (vp .->. vq) .&&. vr    
pruebaFNC_6 = (Negacion ((Negacion (( Negacion vp) .&&. (Negacion vq))) .||. (Negacion (vq .->. vr)))) .&&. (Negacion (vr .->. vp))    
pruebaFNC_7 = (vp .||. (Negacion ((Negacion vq) .||. (Negacion vs)))) .||. (Negacion ((Negacion vq) .->. (Negacion vs)))     
pruebaFNC_8 = ((Negacion (vq .&&. vr)) .||. vp) .<->. ((vp .->. vq) .->. vr)      
pruebaFNC_9 = (vp .&&. (Negacion vq) .&&. vr) .||. ((vp .->. (Negacion vq)) .<->. (vq .<->. vr))     

fnc_result4 = (fnc) (pruebaFNC_4)
fnc_result5 = (fnc) (pruebaFNC_5)
fnc_result6 = (fnc) (pruebaFNC_6)
fnc_result7 = (fnc) (pruebaFNC_7)
fnc_result8 = (fnc) (pruebaFNC_8)
fnc_result9 = (fnc) (pruebaFNC_9)

fnc_test4 = pruebaFNC_4 .<->. fnc_result4
fnc_test5 = pruebaFNC_5 .<->. fnc_result5
fnc_test6 = pruebaFNC_6 .<->. fnc_result6
fnc_test7 = pruebaFNC_7 .<->. fnc_result7
fnc_test8 = pruebaFNC_8 .<->. fnc_result8
fnc_test9 = pruebaFNC_9 .<->. fnc_result9

result_FNC4 = (taut) (fnc_test4)
result_FNC5 = (taut) (fnc_test5)
result_FNC6 = (taut) (fnc_test6)
result_FNC7 = (taut) (fnc_test7)
result_FNC8 = (taut) (fnc_test8)
result_FNC9 = (taut) (fnc_test9)

-- ****** Proposiciones con variables y sin variables que son contradicciones   *******
fnc_contradiccion1 = (Negacion f) .&&. f .||. t .&&. (Negacion t)
fnc_contradiccion2 = (vp .->. (Negacion vp)) .&&. (vq .->. (Negacion vq))

fnc_result10 = (fnc) (fnc_contradiccion1)
fnc_result11 = (fnc) (fnc_contradiccion2)

fnc_test10 = fnc_contradiccion1 .<->. fnc_result10
fnc_test11 = fnc_contradiccion2 .<->. fnc_result11

result_FNC10 = (taut) (fnc_test10)
result_FNC11 = (taut) (fnc_test11)


-- **********************************************
-- *   Para probar la función FND             *
--   Para ver el resultado de la equivalencia se escribe en la consola result_FND<#prueba>
--   #prueba = 1-3 pruebas con proposiciones sin variables
--   #prueba = 4-9 pruebas con proposiciones con variables
--   #prueba = 10-11 pruebas con proposiciones con variables y isn variables que son contradicciones
-- Se pueden ejecutar pruebas: (imprimirProp_Estilo.fnd) (<variable_prueba>)
-- **********************************************

-- *************** Proposiciones sin variables   ****************
fnd_pruebaCons1 = f .->. f .<->. (Negacion f) .||. f
fnd_pruebaCons2 = f .->. t .<->. (Negacion f) .||. t
fnd_pruebaCons3 = f .->. t .<->. (Negacion t) .->. (Negacion f)

pc1FND = (fnd) (fnd_pruebaCons1)
pc2FND = (fnd) (fnd_pruebaCons2)
pc3FND = (fnd) (fnd_pruebaCons3)

fnd_test1 = fnd_pruebaCons1 .<->. pc1FND
fnd_test2 = fnd_pruebaCons2 .<->. pc2FND
fnd_test3 = fnd_pruebaCons3 .<->. pc3FND

result_FND1 = (taut) (fnd_test1)
result_FND2 = (taut) (fnd_test2)
result_FND3 = (taut) (fnd_test3)

-- *************** Proposiciones con variables   ****************

pruebaFND_4 = ((vp .&&. vq) .||. ((Negacion vp) .||. vr )) .||. (vq .&&. vr)   
pruebaFND_5 = (vp .->. vq) .&&. vr    
pruebaFND_6 = (Negacion ((Negacion (( Negacion vp) .&&. (Negacion vq))) .||. (Negacion (vq .->. vr)))) .&&. (Negacion (vr .->. vp))    
pruebaFND_7 = (vp .||. (Negacion ((Negacion vq) .||. (Negacion vs)))) .||. (Negacion ((Negacion vq) .->. (Negacion vs)))     
pruebaFND_8 = ((Negacion (vq .&&. vr)) .||. vp) .<->. ((vp .->. vq) .->. vr)      
pruebaFND_9 = (vp .&&. (Negacion vq) .&&. vr) .||. ((vp .->. (Negacion vq)) .<->. (vq .<->. vr))     

fnd_result4 = (fnd) (pruebaFND_4)
fnd_result5 = (fnd) (pruebaFND_5)
fnd_result6 = (fnd) (pruebaFND_6)
fnd_result7 = (fnd) (pruebaFND_7)
fnd_result8 = (fnd) (pruebaFND_8)
fnd_result9 = (fnd) (pruebaFND_9)

fnd_test4 = pruebaFND_4 .<->. fnd_result4
fnd_test5 = pruebaFND_5 .<->. fnd_result5
fnd_test6 = pruebaFND_6 .<->. fnd_result6
fnd_test7 = pruebaFND_7 .<->. fnd_result7
fnd_test8 = pruebaFND_8 .<->. fnd_result8
fnd_test9 = pruebaFND_9 .<->. fnd_result9

result_FND4 = (taut) (fnd_test4)
result_FND5 = (taut) (fnd_test5)
result_FND6 = (taut) (fnd_test6)
result_FND7 = (taut) (fnd_test7)
result_FND8 = (taut) (fnd_test8)
result_FND9 = (taut) (fnd_test9)

-- ****** Proposiciones con variables y sin variables que son contradicciones   *******
fnd_contradiccion1 = (Negacion f) .&&. f .||. t .&&. (Negacion t)
fnd_contradiccion2 = (vp .->. (Negacion vp)) .&&. (vq .->. (Negacion vq))

fnd_result10 = (fnd) (fnd_contradiccion1)
fnd_result11 = (fnd) (fnd_contradiccion2)

fnd_test10 = fnd_contradiccion1 .<->. fnd_result10
fnd_test11 = fnd_contradiccion2 .<->. fnd_result11

result_FND10 = (taut) (fnd_test10)
result_FND11 = (taut) (fnd_test11)

-- *****************************************************
-- *   Para probar las funciones de simplificacion   *
-- Para probar la funcion de simplificacion
-- Ejecutar: ejecutarPruebasSimplificacion
-- Para probar la funcion de simplificacion maxima
-- Ejecutar: ejecutarPruebasSimpl
-- *****************************************************

listaPruebasSimplificacion = [
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

listaPruebasSimpl = [
    pruebaFNC_4, 
    pruebaFNC_5,
    pruebaFNC_6,  
    pruebaFNC_7,
    pruebaFNC_8,
    pruebaFNC_9,
    fnc_pruebaCons1,
    fnc_pruebaCons2,
    fnc_pruebaCons3,
    fnc_contradiccion1,
    fnc_contradiccion2,
    pruebaFND_4,
    pruebaFND_5,
    pruebaFND_6,  
    pruebaFND_7,  
    pruebaFND_8,     
    pruebaFND_9]

ejecutarPruebasSimplificacionAux numero [] = return ()
ejecutarPruebasSimplificacionAux numero (x:xs) = do
    let numeroSumado = numero+1
    putStrLn ("Prueba #" ++ (show numero))
    
    putStr ((imprimirProp_Estilo x) ++ " -> Simplificacion -> ")

    putStrLn (imprimirProp_Estilo (simplificacion x))

    putStr "\n"
    
    ejecutarPruebasSimplificacionAux numeroSumado xs

ejecutarPruebasSimplAux numero [] = return ()
ejecutarPruebasSimplAux numero (x:xs) = do
    let numeroSumado = numero+1
    putStrLn ("Prueba #" ++ (show numero))
    
    putStr ((imprimirProp_Estilo x) ++ " -> Simpl max -> ")

    putStr (imprimirProp_Estilo (simpl x) ++ " -> taut -> ")

    putStrLn (tautStr ((simpl x) .<->. x))

    putStr "\n"
    
    ejecutarPruebasSimplAux numeroSumado xs


ejecutarPruebasSimplificacion = ejecutarPruebasSimplificacionAux 1 listaPruebasSimplificacion
ejecutarPruebasSimpl = ejecutarPruebasSimplAux 1 listaPruebasSimpl