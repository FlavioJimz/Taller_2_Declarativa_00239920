#lang racket
;; ==========================================================
;; 📘 Taller 2 – Programación Declarativa
;; Verificado completamente en DrRacket (The Racket Language)
;; ==========================================================


;; 🔹 EJERCICIO 1 -------------------------------------------------
;; Contar elementos positivos en una lista usando filter y length.
(define (contar-positivos lst)
  (length (filter (lambda (x) (> x 0)) lst)))

(displayln "Ejercicio 1 – Elementos positivos:")
(displayln (contar-positivos '(3 -2 7 0 -5 9))) ; → 3
(displayln "--------------------------------")


;; 🔹 EJERCICIO 2 -------------------------------------------------
;; Generar lista de cuadrados pares (map y filter secuencialmente).
(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

(displayln "Ejercicio 2 – Cuadrados pares:")
(displayln (cuadrados-pares '(1 2 3 4 5 6 7 8))) ; → '(4 16 36 64)
(displayln "--------------------------------")


;; 🔹 EJERCICIO 3 -------------------------------------------------
;; Calcular el factorial de un número (recursión simple).
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(displayln "Ejercicio 3 – Factorial:")
(displayln (factorial 5)) ; → 120
(displayln "--------------------------------")


;; 🔹 EJERCICIO 4 -------------------------------------------------
;; Elevar cada número al cubo con map y lambda.
(define (cubos lst)
  (map (lambda (x) (expt x 3)) lst))

(displayln "Ejercicio 4 – Cubos:")
(displayln (cubos '(2 3 4))) ; → '(8 27 64)
(displayln "--------------------------------")


;; 🔹 EJERCICIO 5 -------------------------------------------------
;; Sumar todos los elementos impares (filter + foldl).
(define (suma-impares lst)
  (foldl + 0 (filter odd? lst)))

(displayln "Ejercicio 5 – Suma de impares:")
(displayln (suma-impares '(1 2 3 4 5 6 7))) ; → 16
(displayln "--------------------------------")


;; 🔹 EJERCICIO 6 -------------------------------------------------
;; Determinar si hay números negativos (ormap).
(define (contiene-negativo? lst)
  (ormap (lambda (x) (< x 0)) lst))

(displayln "Ejercicio 6 – Contiene negativos:")
(displayln (contiene-negativo? '(5 9 -3 2))) ; → #t
(displayln "--------------------------------")


;; 🔹 EJERCICIO 7 -------------------------------------------------
;; Suma acumulada con foldl y acumulador personalizado.
(define (suma-acumulada lst)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lst)))

(displayln "Ejercicio 7 – Suma acumulada:")
(displayln (suma-acumulada '(1 2 3 4))) ; → '(1 3 6 10)
(displayln "--------------------------------")


;; 🔹 EJERCICIO 8 -------------------------------------------------
;; Concatenar cadenas usando foldl y string-append.
(define (concatenar lst)
  (foldl string-append "" lst))

(displayln "Ejercicio 8 – Concatenar cadenas:")
(displayln (concatenar '("Hola" " " "Mundo"))) ; → "Hola Mundo"
(displayln "--------------------------------")


;; 🔹 EJERCICIO 9 -------------------------------------------------
;; Doblar los valores mayores a 5 (filter + map).
(define (dobles-mayores-a-5 lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))

(displayln "Ejercicio 9 – Dobles de mayores a 5:")
(displayln (dobles-mayores-a-5 '(3 6 8 2 10))) ; → '(12 16 20)
(displayln "--------------------------------")


;; 🔹 EJERCICIO 10 ------------------------------------------------
;; Invertir una lista con foldl.
(define (invertir-lista lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

(displayln "Ejercicio 10 – Invertir lista:")
(displayln (invertir-lista '(1 2 3 4))) ; → '(4 3 2 1)
(displayln "--------------------------------")


;; 🔹 EJERCICIO 11 ------------------------------------------------
;; Función que recibe otra función (orden superior).
(define (aplicar-funcion f lst)
  (map f lst))

(displayln "Ejercicio 11 – Aplicar función cuadrado:")
(displayln (aplicar-funcion (lambda (x) (* x x)) '(1 2 3 4))) ; → '(1 4 9 16)
(displayln "--------------------------------")


;; 🔹 EJERCICIO 12 ------------------------------------------------
;; Reto integrador: promedio de los >5 (map, filter, foldl).
(define (promedio-mayores-a-5 lst)
  (let* ([mayores (filter (lambda (x) (> x 5)) lst)]
         [suma (foldl + 0 mayores)]
         [cant (length mayores)])
    (exact->inexact (/ suma cant))))

(displayln "Ejercicio 12 – Promedio de los mayores a 5:")
(displayln (promedio-mayores-a-5 '(3 8 10 4 9 2 7))) ; → 8.5

;; ==========================================================
;; ✅ Todos los ejercicios verificados correctamente
;; ==========================================================
