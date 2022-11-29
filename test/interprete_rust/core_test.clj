(ns interprete-rust.core-test
  (:require [clojure.test :refer :all]
            [interprete-rust.core :refer :all]))

(deftest listar-test
  (testing "Test del valor de retorno"
    (is (= nil (listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}")))))
  )

  (testing "Test del efecto colateral"
    (is (= "fn main ( )\n{\n  println! ( \"Hola, mundo!\" )\n}\n" (with-out-str (listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}"))))))
  )
)

(deftest agregar-ptocoma-test
  (testing "Test del valor de retorno"
    (is (= (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") (symbol ";") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}")) (agregar-ptocoma (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}")))))
  )
)

(deftest palabra-reservada?-test
  (testing "Test del valor de retorno"
    (is (= true (palabra-reservada? 'while)))
    (is (= false (palabra-reservada? 'until)))
    (is (= false (palabra-reservada? 13)))
  )
)

(deftest identificador?-test
  (testing "Test del valor de retorno"
    (is (= true (identificador? 'boolean)))
    (is (= false (identificador? 'bool)))
    (is (= true (identificador? 'e120)))
    (is (= false (identificador? '12e0)))
    (is (= true (identificador? 'mostrar_salida)))
  )
)

(deftest dump-test
  (testing "Test del valor de retorno"
    (is (= nil (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG])))
    (is (= nil (dump '[HLT])))
    (is (= nil (dump nil)))
  )

  (testing "Test del efecto colateral"
    (is (= "0 [POPREF 2]\n1 [PUSHFI 2]\n2 MUL\n3 [PUSHFI 1]\n4 ADD\n5 NEG\n" (with-out-str (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG]))))
    (is (= "0 HLT\n" (with-out-str (dump '[HLT]))))
    (is (= "0 nil\n" (with-out-str (dump nil))))
  )
)

(deftest ya-declarado-localmente?-test
  (testing "Test del valor de retorno"
    (is (= true (ya-declarado-localmente? 'Write [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))
    (is (= false (ya-declarado-localmente? 'Read [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))
    (is (= true (ya-declarado-localmente? 'Write [[0 1] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))
    (is (= false (ya-declarado-localmente? 'Write [[0 2] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])))
  )
)

(deftest cargar-const-en-tabla-test
  (testing "Test del valor de retorno"
    (let [simb-actual (symbol ";"),
          simb-no-parseados-aun (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")),
          simb-ya-parseados '['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3],
          primer-estado 8,
          segundo-estado :sin-errores,
          contexto '[[0] [['io ['lib '()] 0]]],
          prox-var 0,
          bytecode '[['CAL 0] 'HLT],
          mapa-regs-de-act '[],
          segundo-contexto-esperado '[[0] [['io ['lib '()] 0] ['TRES ['const 'i64] 3]]]]
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act] (cargar-const-en-tabla [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act])))
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado segundo-contexto-esperado prox-var bytecode mapa-regs-de-act]  (cargar-const-en-tabla [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado contexto prox-var bytecode mapa-regs-de-act])))
    )
  )
)

(deftest inicializar-contexto-local-test
  (let [simb-actual (symbol "{"),
        simb-no-parseados-aun (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")),
        simb-ya-parseados '['fn 'main (symbol "(") (symbol ")")],
        primer-estado 8,
        segundo-estado :sin-errores,
        contexto '[[0] [['main ['fn [() ()]] 2]]],
        prox-var 0,
        bytecode '[['CAL 2] 'HLT],
        mapa-regs-de-act '[],
        segundo-contexto-esperado '[[0 1] [['main ['fn [() ()]] 2]]]]
    (testing "Test del valor de retorno"
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act] (inicializar-contexto-local [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act])))
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado segundo-contexto-esperado prox-var bytecode mapa-regs-de-act] (inicializar-contexto-local [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado contexto prox-var bytecode mapa-regs-de-act])))
    )
  )
)

(deftest restaurar-contexto-anterior-test
  (let [simb-actual 'EOF,
        simb-no-parseados-aun '(),
        simb-ya-parseados '['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")],
        primer-estado 8,
        segundo-estado :sin-errores,
        contexto '[[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]],
        prox-var 2,
        bytecode '[['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL],
        mapa-regs-de-act '[[2 ['i64 nil] ['i64 nil]]],
        segundo-contexto-esperado '[[0] [['main ['fn [() ()]] 2]]]]
    (testing "Test del valor de retorno"
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act] (restaurar-contexto-anterior [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act])))
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado segundo-contexto-esperado prox-var bytecode mapa-regs-de-act] (restaurar-contexto-anterior [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado contexto prox-var bytecode mapa-regs-de-act])))
    )
  )
)

(deftest buscar-tipo-de-retorno-test
  (testing "Test del valor de retorno"
    (is (= 'i64) (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 2))
    (is (= '() (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 8)))
    (is (= nil (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 1)))
  )
)

(deftest generar-ref-test
  (let [simb-actual (symbol ")"),
        simb-no-parseados-aun (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")),
        simb-ya-parseados ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v],
        primer-estado 8,
        segundo-estado :sin-errores,
        contexto [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]],
        prox-var 1,
        bytecode [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]],
        mapa-regs-de-act [[2 ['i64 nil]] [6 ['i64 nil]]],
        segundo-bytecode-esperado [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0] ['PUSHADDR 0]]]
    (testing "Test del valor de retorno"
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act] (generar-ref [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act])))
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado contexto prox-var segundo-bytecode-esperado mapa-regs-de-act] (generar-ref [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado contexto prox-var bytecode mapa-regs-de-act])))
    )
  )
)

(deftest fixup-test
  (let [simb-actual (symbol "{"),
        simb-no-parseados-aun (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")),
        simb-ya-parseados ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else],
        primer-estado 8,
        segundo-estado :sin-errores,
        contexto [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]],
        prox-var 1,
        bytecode [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]],
        mapa-regs-de-act [[2 ['i64 nil]]],
        segundo-bytecode-esperado [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP 8] ['PUSHFI 10] ['POP 0] ['JMP '?]]]
    (testing "Test del valor de retorno"
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act] (fixup [simb-actual simb-no-parseados-aun simb-ya-parseados primer-estado contexto prox-var bytecode mapa-regs-de-act] 4)))
      (is (= [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado contexto prox-var segundo-bytecode-esperado mapa-regs-de-act] (fixup [simb-actual simb-no-parseados-aun simb-ya-parseados segundo-estado contexto prox-var bytecode mapa-regs-de-act] 4)))
    )
  )
)

(deftest convertir-formato-impresion-test
  (testing "Test del valor de retorno"
    (is (= '("Hola, mundo!") (convertir-formato-impresion '("Hola, mundo!"))))
    (is (= '("- My name is %s, James %s.\n- Hello, %d%d%d!" "Bond" "Bond" 0 0 7) (convertir-formato-impresion '("- My name is {}, James {}.\n- Hello, {}{}{}!" "Bond" "Bond" 0 0 7))))
    (is (= '("%.0f elevado a la %d es\t%.0f" 2.0 2 4.0) (convertir-formato-impresion '("{} elevado a la {} es\t{}" 2.0 2 4.0))))
    (is (= '("Las raices cuadradas de %.0f son +%.8f y -%.8f" 4.0 1.999999999985448 1.999999999985448) (convertir-formato-impresion '("Las raices cuadradas de {} son +{:.8} y -{:.8}" 4.0 1.999999999985448 1.999999999985448))))
  )
)

(deftest dividir-test
  (testing "Test del valor de retorno"
    (is (= 4 (dividir 12 3)))
    (is (= 4.0 (dividir 12.0 3)))
    (is (= 4.0 (dividir 12 3.0)))
    (is (= 4.0 (dividir 12.0 3.0)))
    (is (= 0 (dividir 1 2)))
    (is (= 0.5 (dividir 1 2.0)))
  )
)

(deftest compatibles?-test
  (testing "Test del valor de retorno"
    (is (= true (compatibles? 'i64 5)))
    (is (= false (compatibles? 'i64 5.0)))
    (is (= true (compatibles? 'i64 [5.0])))
    (is (= true (compatibles? 'f64 5.0)))
    (is (= true (compatibles? 'String "Hola")))
    (is (= true (compatibles? 'bool true)))
    (is (= false (compatibles? 'bool 1)))
    (is (= true (compatibles? 'usize 1)))
    (is (= true (compatibles? 'char \a)))
    (is (= false (compatibles? 'char 'a)))
    (is (= true (compatibles? 'char ['a])))
  )
)

(deftest pasar-a-int-test
  (testing "Test del valor de retorno"
    (is (= 10 (pasar-a-int "10")))
    (is (= 10 (pasar-a-int 10.0)))
    (is (= 10 (pasar-a-int 10)))
    (is (= 'a (pasar-a-int 'a)))
    (is (= [10.0] (pasar-a-int [10.0])))
  )
)

(deftest pasar-a-float-test
  (testing "Test del valor de retorno"
    (is (= 10.0 (pasar-a-float "10")))
    (is (= 10.0 (pasar-a-float 10)))
    (is (= 10.0 (pasar-a-float 10.0)))
    (is (= 'a (pasar-a-float 'a)))
    (is (= [10] (pasar-a-float [10])))
  )
)

(deftest cargar-en-ult-reg-test
  (testing "Test del valor de retorno"
    (is (= [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]] (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 nil]]] 1 'i64 0)))
    (is (= [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['f64 3] ['i64 0]]] (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]] 0 'f64 3)))
  )
)

(deftest cargar-en-reg-dest-test
  (testing "Test del valor de retorno"
    (is (= [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 2]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 4] 'i64 0)))
    (is (= [[['String "2"] ['i64 6] ['i64 2] ['f64 3] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 3] 'f64 3)))
  )
)
