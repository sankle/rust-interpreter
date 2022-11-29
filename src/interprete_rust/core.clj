(ns interprete-rust.core
  (:gen-class))

(defn spy
  ([x] (do (prn x) x))
  ([msg x] (do (print msg) (print ": ") (prn x) x)))

(declare driver-loop)
(declare escanear-arch)
(declare listar)
(declare buscar-mensaje)
(declare agregar-ptocoma)
(declare escanear)
(declare dar-error)
(declare parsear)
(declare simb-actual)
(declare simb-no-parseados-aun)
(declare simb-ya-parseados)
(declare estado)
(declare contexto)
(declare prox-var)
(declare bytecode)
(declare mapa-regs-de-act)
(declare palabra-reservada?)
(declare booleano?)
(declare numero?)
(declare identificador?)
(declare cadena?)
(declare procesar-terminal)
(declare generar)
(declare dump)
(declare ya-declarado-localmente?)
(declare controlar-duplicado)
(declare buscar-coincidencias)
(declare verificar-que-sea)
(declare verificar-que-sea-var)
(declare verificar-que-sea-var-mut)
(declare verificar-que-sea-var-ref)
(declare verificar-que-sea-fn)
(declare verificar-que-sea-string)
(declare verificar-que-sea-const-o-var)
(declare cargar-lib-en-tabla)
(declare cargar-const-en-tabla)
(declare cargar-fn-en-tabla)
(declare cargar-var-mut-en-tabla)
(declare cargar-var-inmut-en-tabla)
(declare inicializar-contexto-global)
(declare inicializar-contexto-local)
(declare restaurar-contexto-anterior)
(declare preparar-mapa-regs-de-act)
(declare programa)
(declare procesar-opcional-declaraciones-use)
(declare procesar-opcional-declaraciones-const)
(declare procesar-opciones-use-std)
(declare procesar-opcional-en-std-io)
(declare declaracion-use)
(declare declaracion-const)
(declare procesar-mas-param)
(declare procesar-tipo-param)
(declare procesar-opcional-mut-param)
(declare declarar-opcional-param)
(declare procesar-tipo-retorno)
(declare declarar-mas-fn)
(declare cargar-params-en-tabla)
(declare hacer-fixup-si-es-main)
(declare hace-push-implicito?)
(declare buscar-tipo-de-retorno)
(declare confirmar-retorno)
(declare confirmar-no-retorno)
(declare declarar-opcional-tipo-retorno)
(declare declarar-fn)
(declare procesar-declaraciones-fn)
(declare procesar-mas-opcional-bloque)
(declare procesar-opcional-mut-let)
(declare procesar-tipo-const)
(declare procesar-opcional-asignacion)
(declare procesar-tipo-variable)
(declare procesar-opcional-bloque)
(declare bloque)
(declare procesar-stdout-stdin)
(declare procesar-opcional-expresiones-a-imprimir)
(declare generar-pushfi-cadena)
(declare procesar-opcional-cadena-expresiones-a-imprimir)
(declare procesar-string-new-from)
(declare procesar-string-punto-as-str-trim)
(declare procesar-to-string)
(declare procesar-parse)
(declare procesar-string-punto-to-string-parse)
(declare procesar-trim)
(declare procesar-as-str)
(declare procesar-opcional-string-punto)
(declare procesar-tipo-as)
(declare procesar-opcional-as)
(declare procesar-f64-funcion)
(declare procesar-ident-punto-parse-to-string)
(declare procesar-ident-punto)
(declare procesar-opcional-as-punto)
(declare procesar-mas-opcional-expresion)
(declare procesar-opcional-expresion)
(declare generar-factor-const-o-var)
(declare procesar-opcional-asignacion-llamada)
(declare generar-ref)
(declare expresion-atomica)
(declare expresion-unaria)
(declare procesar-mas-expresion-unaria)
(declare expresion-multiplicativa)
(declare procesar-mas-expresion-multiplicativa)
(declare expresion-aditiva)
(declare procesar-mas-expresion-aditiva)
(declare expresion-relacional)
(declare procesar-mas-expresion-relacional)
(declare expresion-igualdad)
(declare procesar-mas-expresion-igualdad)
(declare expresion-and)
(declare procesar-mas-expresion-and)
(declare expresion)
(declare fixup)
(declare generar-operador-relacional)
(declare generar-con-valor)
(declare convertir-formato-impresion)
(declare dividir)
(declare aplicar-operador-monadico)
(declare aplicar-operador-diadico)
(declare compatibles?)
(declare pasar-a-int)
(declare pasar-a-float)
(declare cargar-en-ult-reg)
(declare cargar-en-reg-dest)
(declare asignar-aritmetico)
(declare asignar-aritmetico-ref)
(declare params-args)
(declare generar-print!)
(declare generar-format!)
(declare interpretar)

(defn -main
  "Runs driver-loop."
  [& args]
  (driver-loop))

(defn driver-loop
  ([]
   (prn)
   (println "Interprete de RUST en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales - 2022")
   (println)
   (println "Inspirado en: rustc 1.64.0 (2022-09-22)")
   (prn)
   (println "Lista de comandos posibles:")
   (println "AYUDA: volver a este menu")
   (println "SALIR: volver al REPL de Clojure")
   (println "ESCAN <archivo>: mostrar los tokens de un programa escrito en Rust")
   (println "VIRTU <archivo>: mostrar la RI de un programa escrito en Rust")
   (println "INTER <archivo>: interpretar la RI de un programa escrito en Rust")
   (prn)
   (driver-loop :iniciado))
  ([status]
   (print "Rust> ") (flush)
   (try (let [linea (clojure.string/split (clojure.string/trim (read-line)) #" "),
              cabeza (clojure.string/upper-case (first linea))]
          (cond
            (= cabeza "SALIR") 'CHAU
            (= cabeza "AYUDA") (driver-loop)
            (= cabeza "ESCAN") (let [nom (second linea)]
                                 (if (not (.exists (clojure.java.io/file nom)))
                                   (do (print "ERROR: ") (println (buscar-mensaje 2) (str (symbol "(") nom (symbol ")"))) (flush) (driver-loop status))
                                   (do (listar (escanear-arch nom)) (driver-loop status))))
            (= cabeza "VIRTU") (let [nom (second linea)]
                                 (if (not (.exists (clojure.java.io/file nom)))
                                   (do (print "ERROR: ") (println (buscar-mensaje 2)) (flush) (driver-loop status))
                                   (let [res (parsear (agregar-ptocoma (escanear-arch nom)))]
                                     (do (if (= (estado res) :sin-errores)
                                           (dump (bytecode res)))
                                         (driver-loop status)))))
            (= cabeza "INTER") (let [nom (second linea)]
                                 (if (not (.exists (clojure.java.io/file nom)))
                                   (do (print "ERROR: ") (println (buscar-mensaje 2)) (flush) (driver-loop status))
                                   (let [res (parsear (agregar-ptocoma (escanear-arch nom)))]
                                     (do (if (= (estado res) :sin-errores)
                                           (interpretar (bytecode res) [] 0 [] (mapa-regs-de-act res)))
                                                           ; IMPORTANTE
                                                           ; Este es el AMBIENTE inicial del interprete en su fase interpretativa.
                                                           ; [cod  regs-de-act  cont-prg  pila  mapa-regs]
                                         (driver-loop status)))))
            (= cabeza "") (driver-loop status)
            :else (do (print "ERROR: ") (println (buscar-mensaje 1) (str (symbol "(") (first linea) (symbol ")"))) (flush) (driver-loop status))))
        (catch Exception e (println "ERROR ->" (clojure.string/trim (clojure.string/upper-case (let [msg-err (get (Throwable->map e) :cause)] (if (nil? msg-err) "desconocido" msg-err))))) (driver-loop status)))))

(defn escanear-arch [nom]
  (map #(let [aux (try (clojure.edn/read-string %) (catch Exception e (symbol %)))] (if (or (number? aux) (string? aux) (instance? Boolean aux)) aux (symbol %)))
       (remove empty? (with-open [rdr (clojure.java.io/reader nom)]
                        (flatten (doall (map #(re-seq #"print!|println!|format!|\:\:|\<\=|\>\=|\-\>|\=\=|\!\=|\+\=|\-\=|\*\=|\/\=|\%\=|\&\&|\|\||\<|\>|\=|\(|\)|\,|\;|\+|\-|\*|\/|\[|\]|\{|\}|\%|\&|\!|\:|\"[^\"]*\"|\d+\.\d+E[+-]?\d+|\d+\.E[+-]?\d+|\.\d+E[+-]?\d+|\d+E[+-]?\d+|\d+\.\d+|\d+\.|\.\d+|\.|\d+|\_[A-Za-z0-9\_]+|[A-Za-z][A-Za-z0-9\_]*|\.|\'|\"|\||\#|\$|\@|\?|\^|\\|\~" %) (line-seq rdr))))))))

(defn buscar-mensaje [cod]
  (case cod
    1 "COMANDO DESCONOCIDO"
    2 "ARCHIVO NO ENCONTRADO"
    3 "SE ENCONTRO PREMATURAMENTE EL FIN DEL ARCHIVO:  EOF"
    4 "SE ESPERABA std"
    5 "SE ESPERABA UN OPERADOR DE RESOLUCION DE AMBITO:  ::"
    6 "SE ESPERABA LA PALABRA RESERVADA io"
    7 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS Write O process"
    8 "SE ESPERABA UN PUNTO Y COMA:  ;"
    9 "SE ESPERABA UN IGUAL:  ="
    10 "SE ESPERABA UN IDENTIFICADOR"
    11 "SE ESPERABA ABRIR UN PARENTESIS:  ("
    12 "SE ESPERABA CERRAR UN PARENTESIS:  )"
    13 "SE ESPERABA UN TIPO DE RETORNO:  i64, f64, bool O String"
    14 "SE ESPERABAN DOS PUNTOS:  :"
    15 "SE ESPERABA UN TIPO DE PARAMETRO:  i64 O f64"
    16 "SE ESPERABA LA PALABRA RESERVADA mut"
    17 "SE ESPERABA ABRIR UNA LLAVE:  {"
    18 "SE ESPERABA UN PUNTO Y COMA O CERRAR UNA LLAVE:  ; O }"
    19 "SE ESPERABA UN TIPO DE VARIABLE:  i64, f64, bool, char O String"
    20 "SE ESPERABA UN TIPO DE CONSTANTE:  i64 O f64"
    21 "SE ESPERABA UN INICIO DE EXPRESION"
    22 "SE ESPERABA LA PALABRA RESERVADA exit"
    23 "SE ESPERABA UN PUNTO:  ."
    24 "SE ESPERABA LA PALABRA RESERVADA expect"
    25 "SE ESPERABA UNA CADENA"
    26 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS stdin O stdout"
    27 "SE ESPERABA LA PALABRA RESERVADA flush"
    28 "SE ESPERABA LA PALABRA RESERVADA read_line"
    29 "SE ESPERABA UN AMPERSAND:  &"
    30 "SE ESPERABA UN IDENTIFICADOR O LA PALABRA RESERVADA mut"
    31 "SE ESPERABA LA PALABRA RESERVADA chars"
    32 "SE ESPERABA LA PALABRA RESERVADA nth"
    33 "SE ESPERABA LA PALABRA RESERVADA unwrap"
    34 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS new O from"
    35 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS as_str O trim"
    36 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS to_string O parse"
    37 "SE ESPERABA UN TIPO NUMERICO:  i64, f64 O usize"
    38 "SE ESPERABA UNA FUNCION MATEMATICA:  sqrt, sin, atan O abs"
    39 "SE ESPERABA UN NUMERO"
    40 "SE ESPERABA LA PALABRA RESERVADA fn"
    41 "DECLARACION DUPLICADA DE IDENTIFICADOR DE BIBLIOTECA, CONSTANTE O FUNCION"
    42 "IDENTIFICADOR NO DECLARADO"
    43 "SE ESPERABA UN IDENTIFICADOR DE VARIABLE"
    44 "SE ESPERABA UN IDENTIFICADOR DE VARIABLE MUTABLE"
    45 "SE ESPERABA UN IDENTIFICADOR DE FUNCION"
    46 "SE ESPERABA UN IDENTIFICADOR DE CONSTANTE O VARIABLE"
    47 "SE ESPERABA QUE LA FUNCION RETORNE UN VALOR"
    48 "SE ESPERABA QUE LA FUNCION NO RETORNE NINGUN VALOR"
    49 "SE ESPERABA UNA COMA O CERRAR UN PARENTESIS:  , O )"
    50 "TIPOS INCOMPATIBLES"
    51 "SE ESPERABA UN IDENTIFICADOR DE VARIABLE DE TIPO String"
    52 "SE ESPERABA ABRIR UN CORCHETE ANGULAR:  <"
    53 "SE ESPERABA CERRAR UN CORCHETE ANGULAR:  >"
    54 "SE ESPERABA UN IDENTIFICADOR DE PUNTERO"
    55 "FALLO EN UNA OPERACION MONADICA"
    56 "FALLO EN UNA OPERACION DIADICA"
    cod))

(defn escanear [amb]
  (if (= (estado amb) :sin-errores)
    [(let [simb (first (simb-no-parseados-aun amb))]
       (if (nil? simb) 'EOF simb)) (rest (simb-no-parseados-aun amb)) (conj (simb-ya-parseados amb) (simb-actual amb)) (estado amb) (contexto amb) (prox-var amb) (bytecode amb) (mapa-regs-de-act amb)]
    amb))

(defn dar-error [amb cod]
  (if (= (estado amb) :sin-errores)
    (do (prn)
        (println "ERROR AL INTERPRETAR EL PROGRAMA!")
        (println "*********************************")
        (prn)
        (listar (simb-ya-parseados amb))
        (prn) (println ">") (println ">>" (buscar-mensaje cod)) (println ">") (prn)
        (pr (simb-actual amb)) (print " ")
        (listar (simb-no-parseados-aun amb)) (prn)
        (flush)
        [(simb-actual amb) '() (simb-ya-parseados amb) cod])
    amb))

(defn parsear [tokens]
  (let [simbolo-inicial (first tokens)]
    (if (nil? simbolo-inicial)
      (dar-error ['EOF '() [] :sin-errores] 3)
      (programa [simbolo-inicial (rest tokens) [] :sin-errores [] 0 [] []])))
           ; IMPORTANTE
           ; Este es el AMBIENTE inicial del interprete en su fase compilativa.
           ; [simb-actual  simb-no-parseados-aun  simb-ya-parseados  estado  contexto  prox-var  bytecode mapa-regs-de-act]
  )

(defn simb-actual [amb]
  (amb 0))

(defn simb-no-parseados-aun [amb]
  (amb 1))

(defn simb-ya-parseados [amb]
  (amb 2))

(defn estado [amb]
  (amb 3))

(defn contexto [amb]
  (amb 4))

(defn prox-var [amb]
  (amb 5))

(defn bytecode [amb]
  (amb 6))

(defn mapa-regs-de-act [amb]
  (amb 7))

(defn booleano? [x]
  (contains? #{"true" "false"} (str x)))

(defn numero? [x]
  (number? x))

(defn cadena? [x]
  (string? x))

(defn procesar-terminal [amb x cod-err]
  (if (= (estado amb) :sin-errores)
    (if (or (and (symbol? x) (= (simb-actual amb) x)) (x (simb-actual amb)))
      (escanear amb)
      (dar-error amb cod-err))
    amb))

(defn generar
  ([amb instr]
   (if (= (estado amb) :sin-errores)
     (assoc amb 6 (conj (bytecode amb) instr))
     amb))
  ([amb instr val]
   (if (= (estado amb) :sin-errores)
     (assoc amb 6 (conj (bytecode amb) [instr val]))
     amb)))

(defn controlar-duplicado [amb]
  (if (= (estado amb) :sin-errores)
    (if (ya-declarado-localmente? (last (simb-ya-parseados amb)) (contexto amb))
      (let [coincidencias (buscar-coincidencias amb),
            tipo (second (last coincidencias))]
        (if (not (contains? (hash-set 'var-mut 'var-inmut) tipo))
          (dar-error amb 41)
          amb))
      amb)
    amb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recibe un ambiente y devuelve una lista con las ternas [identificador, tipo, valor] provenientes del segundo
; subvector del vector contexto que tengan como identificador al ultimo simbolo ubicado en el vector de simbolos ya
; escaneados.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn buscar-coincidencias
  ([amb] (filter #(= (first %) (last (simb-ya-parseados amb))) (second (contexto amb))))
  ([amb elem] (filter #(= (first %) elem) (second (contexto amb)))))

(defn verificar-que-sea [amb fn-control]
  (if (= (estado amb) :sin-errores)
    (let [coincidencias (buscar-coincidencias amb)]
      (if (empty? coincidencias)
        (dar-error amb 42)
        (fn-control amb coincidencias)))
    amb))

(defn verificar-que-sea-var [amb]
  (verificar-que-sea amb #(if (not (contains? (hash-set 'var-mut 'var-inmut 'var-ref) (first (second (last %2)))))
                            (dar-error %1 43)
                            %1)))

(defn verificar-que-sea-var-mut [amb]
  (verificar-que-sea amb #(if (not (contains? (hash-set 'var-mut 'var-ref) (first (second (last %2)))))
                            (dar-error %1 44)
                            %1)))

(defn verificar-que-sea-var-ref [amb]
  (verificar-que-sea amb #(if (not= 'var-ref (first (second (last %2))))
                            (dar-error %1 54)
                            %1)))

(defn verificar-que-sea-fn [amb]
  (verificar-que-sea amb #(if (not= 'fn (first (second (last %2))))
                            (dar-error %1 45)
                            %1)))

(defn verificar-que-sea-string [amb]
  (verificar-que-sea amb #(if (not= 'String (second (second (last %2))))
                            (dar-error %1 51)
                            %1)))

(defn verificar-que-sea-const-o-var [amb]
  (verificar-que-sea amb #(if (not (contains? (hash-set 'const 'var-mut 'var-inmut 'var-ref) (first (second (last %2)))))
                            (dar-error %1 46)
                            %1)))

(defn cargar-lib-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
    (assoc amb 4 [((contexto amb) 0)
                  (conj ((contexto amb) 1) [(last (simb-ya-parseados amb))
                                            ['lib ()]
                                            0])])
    amb))

(defn cargar-fn-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
    (let [simbolos-ya (simb-ya-parseados amb),
          nombre (second (drop-while #(not= % (symbol "(")) (reverse simbolos-ya))),
          tipo-dato (if (contains? (hash-set 'i64 'f64 'bool 'String) (last simbolos-ya))
                      [(params-args (drop-last 2 simbolos-ya)) (last simbolos-ya)]
                      [(params-args simbolos-ya) ()]),
          valor (count (bytecode amb))]
      (assoc amb 4 [((contexto amb) 0)
                    (conj ((contexto amb) 1) [nombre
                                              ['fn tipo-dato]
                                              valor])]))
    amb))

(defn cargar-var-mut-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
    (let [simbolos-ya (simb-ya-parseados amb),
          nombre (last (butlast (butlast simbolos-ya))),
          tipo-dato (last simbolos-ya),
          valor (prox-var amb),
          nuevo (assoc (assoc amb 4 [((contexto amb) 0)
                                     (conj ((contexto amb) 1) [nombre
                                                               ['var-mut tipo-dato]
                                                               valor])]) 5 (inc valor)),
          vars (mapa-regs-de-act amb)]
      (assoc nuevo 7 (conj (vec (butlast vars)) (conj (last vars) [tipo-dato nil]))))
    amb))

(defn cargar-var-inmut-en-tabla [amb]
  (if (= (estado amb) :sin-errores)
    (let [simbolos-ya (simb-ya-parseados amb),
          nombre (last (butlast (butlast simbolos-ya))),
          tipo-dato (last simbolos-ya),
          valor (prox-var amb),
          nuevo (assoc (assoc amb 4 [((contexto amb) 0)
                                     (conj ((contexto amb) 1) [nombre
                                                               ['var-inmut tipo-dato]
                                                               valor])]) 5 (inc valor)),
          vars (mapa-regs-de-act amb)]
      (assoc nuevo 7 (conj (vec (butlast vars)) (conj (last vars) [tipo-dato nil]))))
    amb))

(defn inicializar-contexto-global [amb]
  (if (= (estado amb) :sin-errores)
    (assoc amb 4 [[0] []])           ; [fronteras  tabla]
    amb))

(defn preparar-mapa-regs-de-act [amb]
  (if (= (estado amb) :sin-errores)
    (assoc amb 7 (zipmap (map first (mapa-regs-de-act amb)) (map vec (map rest (mapa-regs-de-act amb)))))
    amb))

(defn programa [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (inicializar-contexto-global)
        (generar ,,, 'CAL 0)
        (generar ,,, 'HLT)
        (procesar-opcional-declaraciones-use)
        (procesar-opcional-declaraciones-const)
        (procesar-declaraciones-fn)
        (preparar-mapa-regs-de-act))
    amb))

(defn procesar-opcional-declaraciones-use [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) 'use)
      (-> amb
          (escanear)
          (declaracion-use)
          (procesar-terminal ,,, (symbol ";") 8)
          (recur))
      amb)
    amb))

(defn procesar-opcional-declaraciones-const [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) 'const)
      (-> amb
          (escanear)
          (declaracion-const)
          (procesar-terminal ,,, (symbol ";") 8)
          (recur))
      amb)
    amb))

(defn procesar-opciones-use-std [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      io (-> amb
             (escanear)
             (procesar-opcional-en-std-io))
      process (-> amb
                  (escanear))
      (dar-error amb 6))
    amb))

(defn procesar-opcional-en-std-io [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol "::"))
      (-> amb
          (escanear)
          (procesar-terminal ,,, 'Write 7))
      amb)
    amb))

(defn declaracion-use [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (procesar-terminal ,,, 'std 4)
        (procesar-terminal ,,, (symbol "::") 5)
        (procesar-opciones-use-std)
        (controlar-duplicado)
        (cargar-lib-en-tabla))
    amb))

(defn declaracion-const [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (procesar-terminal ,,, identificador? 10)
        (controlar-duplicado)
        (procesar-terminal ,,, (symbol ":") 14)
        (procesar-tipo-const)
        (procesar-terminal ,,, (symbol "=") 9)
        (procesar-terminal ,,, numero? 39)
        (cargar-const-en-tabla))
    amb))

(defn procesar-mas-param [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol ","))
      (-> amb
          (escanear)
          (declarar-opcional-param)
          (recur))
      amb)
    amb))

(defn procesar-tipo-param [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      i64 (-> amb
              (escanear))
      f64 (-> amb
              (escanear))
      (dar-error amb 15))
    amb))

(defn procesar-opcional-mut-param [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol "&"))
      (-> amb
          (escanear)
          (procesar-terminal ,,, 'mut 16))
      amb)
    amb))

(defn declarar-opcional-param [amb]
  (if (= (estado amb) :sin-errores)
    (cond
      (= (simb-actual amb) 'mut)
      (-> amb
          (escanear)
          (procesar-terminal ,,, identificador? 10)
          (procesar-terminal ,,, (symbol ":") 14)
          (procesar-tipo-param)
          (procesar-mas-param))
      (identificador? (simb-actual amb))
      (-> amb
          (escanear)
          (procesar-terminal ,,, (symbol ":") 14)
          (procesar-opcional-mut-param)
          (procesar-tipo-param)
          (procesar-mas-param))
      :else amb)
    amb))

(defn procesar-tipo-retorno [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      i64 (-> amb
              (escanear))
      f64 (-> amb
              (escanear))
      bool (-> amb
               (escanear))
      String (-> amb
                 (escanear))
      (dar-error amb 13))
    amb))

(defn declarar-mas-fn [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) 'fn)
      (-> amb
          (escanear)
          (declarar-fn)
          (recur))
      amb)
    amb))

(defn cargar-params-en-tabla
  ([amb]
   (if (= (estado amb) :sin-errores)
     (let [simbolos-ya (simb-ya-parseados amb),
           params (if (contains? (hash-set 'i64 'f64 'bool 'String) (last simbolos-ya))
                    (params-args (drop-last 2 simbolos-ya))
                    (params-args simbolos-ya)),
           proto-amb (vec (conj (map #(if (empty? %) % [(last %) nil]) params) (count (bytecode amb)))),
           args (map #(conj ['POPARG] %) (range (dec (count params)) -1 -1))]
       (cargar-params-en-tabla (assoc (assoc amb 7 (conj (mapa-regs-de-act amb)
                                                         (if (and (= (count proto-amb) 2) (= (second proto-amb) []))
                                                           [(first proto-amb)]
                                                           proto-amb))) 6 (into (bytecode amb) args))
                               params))
     amb))
  ([amb params]
   (if (empty? params)
     amb
     (let [param (first params),
           tam (count param)]
       (case tam
         0 (cargar-params-en-tabla amb (rest params))
         3 (cargar-params-en-tabla (let [nombre (first param),
                                         tipo-dato (last param),
                                         valor (prox-var amb)]
                                     (assoc (assoc amb 4 [((contexto amb) 0)
                                                          (conj ((contexto amb) 1) [nombre
                                                                                    ['var-inmut tipo-dato]
                                                                                    valor])]) 5 (inc valor))) (rest params))
         4 (cargar-params-en-tabla (let [nombre (second param),
                                         tipo-dato (last param),
                                         valor (prox-var amb)]
                                     (assoc (assoc amb 4 [((contexto amb) 0)
                                                          (conj ((contexto amb) 1) [nombre
                                                                                    ['var-mut tipo-dato]
                                                                                    valor])]) 5 (inc valor))) (rest params))
         5 (cargar-params-en-tabla (let [nombre (first param),
                                         tipo-dato (last param),
                                         valor (prox-var amb)]
                                     (assoc (assoc amb 4 [((contexto amb) 0)
                                                          (conj ((contexto amb) 1) [nombre
                                                                                    ['var-ref tipo-dato]
                                                                                    valor])]) 5 (inc valor))) (rest params)))))))

(defn hacer-fixup-si-es-main [amb]
  (if (= (estado amb) :sin-errores)
    (let [simbolos-ya (simb-ya-parseados amb),
          firma (take-last 3 simbolos-ya)]
      (if (= firma (list 'main (symbol "(") (symbol ")")))
        (let [bc (bytecode amb)]
          (assoc amb 6 (assoc bc 0 ['CAL (count bc)])))
        amb))
    amb))

(defn hace-push-implicito? [instr]
  (contains? #{'ADD 'SUB 'MUL 'DIV 'MOD 'OR 'AND 'EQ 'NEQ 'GT 'GTE 'LT 'LTE 'NEG 'NOT 'SQRT 'SIN 'ATAN 'ABS 'TOI 'TOF} instr))

(defn confirmar-retorno [amb]
  (if (= (estado amb) :sin-errores)
    (let [ultimo (last (bytecode amb)),
          no-simb-ult (not (symbol? ultimo))]
      (cond
        (hace-push-implicito? ultimo) amb
        (and no-simb-ult (= (first ultimo) 'PUSHFM)) amb
        (and no-simb-ult (= (first ultimo) 'PUSHFI)) amb
        (and no-simb-ult (= (first ultimo) 'CAL) (not= (buscar-tipo-de-retorno amb (second ultimo)) ())) amb
        :else (dar-error amb 47)))
    amb))

(defn confirmar-no-retorno [amb]
  (if (= (estado amb) :sin-errores)
    (let [ultimo (last (bytecode amb)),
          no-simb-ult (not (symbol? ultimo))]
      (cond
        (hace-push-implicito? ultimo) (dar-error amb 48)
        (and no-simb-ult (= (first ultimo) 'PUSHFM)) (dar-error amb 48)
        (and no-simb-ult (= (first ultimo) 'PUSHFI)) (dar-error amb 48)
        (and no-simb-ult (= (first ultimo) 'CAL) (not= (buscar-tipo-de-retorno amb (second ultimo)) ())) (dar-error amb 48)
        :else amb))
    amb))

(defn declarar-opcional-tipo-retorno [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol "->"))
      (-> amb
          (escanear)
          (procesar-tipo-retorno)
          (cargar-fn-en-tabla)
          (hacer-fixup-si-es-main)
          (inicializar-contexto-local)
          (cargar-params-en-tabla)
          (bloque)
          (confirmar-retorno)
          (restaurar-contexto-anterior)
          (generar ,,, 'RET))
      (-> amb
          (cargar-fn-en-tabla)
          (hacer-fixup-si-es-main)
          (inicializar-contexto-local)
          (cargar-params-en-tabla)
          (bloque)
          (confirmar-no-retorno)
          (restaurar-contexto-anterior)
          (generar ,,, 'RETN)))
    amb))

(defn declarar-fn [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (assoc ,,, 5 0)
        (procesar-terminal ,,, identificador? 10)
        (controlar-duplicado)
        (procesar-terminal ,,, (symbol "(") 11)
        (declarar-opcional-param)
        (procesar-terminal ,,, (symbol ")") 49)
        (declarar-opcional-tipo-retorno))
    amb))

(defn procesar-declaraciones-fn [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (procesar-terminal ,,, 'fn 9)
        (declarar-fn)
        (declarar-mas-fn))
    amb))

(defn procesar-mas-opcional-bloque [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol ";"))
      (let [ultimo (last (bytecode amb)),
            usar-amb (cond (hace-push-implicito? ultimo)
                           (generar amb 'POP)
                           (and (not (symbol? ultimo)) (= (first ultimo) 'CAL) (not= (buscar-tipo-de-retorno amb (second ultimo)) ()))
                           (generar amb 'POP)
                           (and (not (symbol? ultimo)) (= (first ultimo) 'PUSHFM))
                           (generar amb 'POP)
                           (and (not (symbol? ultimo)) (= (first ultimo) 'PUSHFI))
                           (generar amb 'POP)
                           :else amb)]
        (-> usar-amb
            (escanear)
            (procesar-opcional-bloque)
            (recur)))
      amb)
    amb))

(defn procesar-opcional-mut-let [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) 'mut)
      (-> amb
          (escanear)
          (procesar-terminal ,,, identificador? 10)
          (procesar-terminal ,,, (symbol ":") 14)
          (procesar-tipo-variable)
          (cargar-var-mut-en-tabla)
          (procesar-opcional-asignacion)
          (procesar-mas-opcional-bloque))
      (-> amb
          (procesar-terminal ,,, identificador? 10)
          (procesar-terminal ,,, (symbol ":") 14)
          (procesar-tipo-variable)
          (cargar-var-inmut-en-tabla)
          (procesar-opcional-asignacion)
          (procesar-mas-opcional-bloque)))
    amb))

(defn procesar-tipo-const [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      i64 (-> amb
              (escanear))
      f64 (-> amb
              (escanear))
      (dar-error amb 20))
    amb))

(defn procesar-opcional-asignacion [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol "="))
      (let [direc (dec (prox-var amb))]
        (-> amb
            (escanear)
            (expresion)
            (generar ,,, 'POP direc)))
      amb)
    amb))

(defn procesar-tipo-variable [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      i64 (-> amb
              (escanear))
      f64 (-> amb
              (escanear))
      bool (-> amb
               (escanear))
      char (-> amb
               (escanear))
      String (-> amb
                 (escanear))
      (dar-error amb 19))
    amb))

(defn procesar-opcional-bloque [amb]
  (if (= (estado amb) :sin-errores)
    (cond
      (= (simb-actual amb) (symbol "}"))
      amb
      (= (simb-actual amb) 'let)
      (-> amb
          (escanear)
          (procesar-opcional-mut-let))
      :else
      (-> amb
          (expresion)
          (procesar-mas-opcional-bloque)))
    amb))

(defn bloque [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (procesar-terminal ,,, (symbol "{") 17)
        (procesar-opcional-bloque)
        (procesar-terminal ,,, (symbol "}") 18))
    amb))

(defn procesar-stdout-stdin [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      stdout (-> amb
                 (escanear)
                 (procesar-terminal ,,, (symbol "(") 11)
                 (procesar-terminal ,,, (symbol ")") 12)
                 (procesar-terminal ,,, (symbol ".") 23)
                 (procesar-terminal ,,, 'flush 27)
                 (procesar-terminal ,,, (symbol "(") 11)
                 (procesar-terminal ,,, (symbol ")") 12)
                 (generar ,,, 'FLUSH))
      stdin (-> amb
                (escanear)
                (procesar-terminal ,,, (symbol "(") 11)
                (procesar-terminal ,,, (symbol ")") 12)
                (procesar-terminal ,,, (symbol ".") 23)
                (procesar-terminal ,,, 'read_line 28)
                (procesar-terminal ,,, (symbol "(") 11)
                (procesar-terminal ,,, (symbol "&") 29)
                (procesar-terminal ,,, 'mut 16)
                (procesar-terminal ,,, identificador? 10)
                (verificar-que-sea-var-mut)
                (verificar-que-sea-string)
                (generar-con-valor ,,, 'IN)
                (procesar-terminal ,,, (symbol ")") 12))
      (dar-error amb 26))
    amb))

(defn procesar-opcional-expresiones-a-imprimir [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol ","))
      (-> amb
          (escanear)
          (expresion)
          (recur))
      amb)
    amb))

(defn generar-pushfi-cadena [amb]
  (generar amb 'PUSHFI (last (simb-ya-parseados amb))))

(defn procesar-opcional-cadena-expresiones-a-imprimir [amb]
  (if (= (estado amb) :sin-errores)
    (if (cadena? (simb-actual amb))
      (-> amb
          (escanear)
          (generar-pushfi-cadena)
          (procesar-opcional-expresiones-a-imprimir))
      amb)
    amb))

(defn procesar-string-new-from [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      new (-> amb
              (escanear)
              (procesar-terminal ,,, (symbol "(") 11)
              (procesar-terminal ,,, (symbol ")") 12)
              (generar ,,, 'PUSHFI ""))
      from (-> amb
               (escanear)
               (procesar-terminal ,,, (symbol "(") 11)
               (procesar-terminal ,,, cadena? 25)
               (generar-pushfi-cadena)
               (procesar-terminal ,,, (symbol ")") 12))
      (dar-error amb 34))
    amb))

(defn procesar-string-punto-as-str-trim [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      as_str (-> amb
                 (escanear)
                 (procesar-as-str))
      trim (-> amb
               (escanear)
               (procesar-trim)
               (procesar-string-punto-to-string-parse))
      (dar-error amb 35))
    amb))

(defn procesar-to-string [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (procesar-terminal ,,, (symbol "(") 11)
        (procesar-terminal ,,, (symbol ")") 12))
    amb))

(defn procesar-parse [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (procesar-terminal ,,, (symbol "::") 5)
        (procesar-terminal ,,, (symbol "<") 52)
        (procesar-tipo-as)
        (procesar-terminal ,,, (symbol ">") 53)
        (procesar-terminal ,,, (symbol "(") 11)
        (procesar-terminal ,,, (symbol ")") 12)
        (procesar-terminal ,,, (symbol ".") 23)
        (procesar-terminal ,,, 'expect 24)
        (procesar-terminal ,,, (symbol "(") 11)
        (procesar-terminal ,,, cadena? 25)
        (procesar-terminal ,,, (symbol ")") 12))
    amb))

(defn procesar-string-punto-to-string-parse [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      to_string (-> amb
                    (escanear)
                    (procesar-to-string))
      parse (-> amb
                (escanear)
                (procesar-parse))
      (dar-error amb 36))
    amb))

(defn procesar-trim [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (procesar-terminal ,,, (symbol "(") 11)
        (procesar-terminal ,,, (symbol ")") 12)
        (procesar-terminal ,,, (symbol ".") 23))
    amb))

(defn procesar-as-str [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (procesar-terminal ,,, (symbol "(") 11)
        (procesar-terminal ,,, (symbol ")") 12)
        (procesar-terminal ,,, (symbol ".") 23)
        (procesar-terminal ,,, 'chars 31)
        (procesar-terminal ,,, (symbol "(") 11)
        (procesar-terminal ,,, (symbol ")") 12)
        (procesar-terminal ,,, (symbol ".") 23)
        (procesar-terminal ,,, 'nth 32)
        (procesar-terminal ,,, (symbol "(") 11)
        (expresion)
        (procesar-terminal ,,, (symbol ")") 12)
        (procesar-terminal ,,, (symbol ".") 23)
        (procesar-terminal ,,, 'unwrap 33)
        (procesar-terminal ,,, (symbol "(") 11)
        (procesar-terminal ,,, (symbol ")") 12)
        (generar ,,, 'CHR))
    amb))

(defn procesar-opcional-string-punto [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol "."))
      (-> amb
          (escanear)
          (procesar-string-punto-as-str-trim))
      amb)
    amb))

(defn procesar-tipo-as [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      i64 (-> amb
              (escanear)
              (generar ,,, 'TOI))
      f64 (-> amb
              (escanear)
              (generar ,,, 'TOF))
      usize (-> amb
                (escanear)
                (generar ,,, 'TOI))
      (dar-error amb 37))
    amb))

(defn procesar-opcional-as [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) 'as)
      (-> amb
          (escanear)
          (procesar-tipo-as))
      amb)
    amb))

(defn procesar-f64-funcion [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      sqrt (-> amb
               (escanear)
               (procesar-terminal ,,, (symbol "(") 11)
               (expresion)
               (procesar-terminal ,,, (symbol ")") 12)
               (generar ,,, 'SQRT))
      sin (-> amb
              (escanear)
              (procesar-terminal ,,, (symbol "(") 11)
              (expresion)
              (procesar-terminal ,,, (symbol ")") 12)
              (generar ,,, 'SIN))
      atan (-> amb
               (escanear)
               (procesar-terminal ,,, (symbol "(") 11)
               (expresion)
               (procesar-terminal ,,, (symbol ")") 12)
               (generar ,,, 'ATAN))
      abs (-> amb
              (escanear)
              (procesar-terminal ,,, (symbol "(") 11)
              (expresion)
              (procesar-terminal ,,, (symbol ")") 12)
              (generar ,,, 'ABS))
      (dar-error amb 38))
    amb))

(defn procesar-ident-punto-parse-to-string [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      parse (-> amb
                (escanear)
                (procesar-parse))
      to_string (-> amb
                    (escanear)
                    (procesar-to-string))
      (dar-error amb 36))
    amb))

(defn procesar-ident-punto [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      as_str (-> amb
                 (escanear)
                 (procesar-as-str))
      trim (-> amb
               (escanear)
               (procesar-trim)
               (procesar-ident-punto-parse-to-string))
      (dar-error amb 35))
    amb))

(defn procesar-opcional-as-punto [amb]
  (if (= (estado amb) :sin-errores)
    (cond
      (= (simb-actual amb) 'as)
      (-> amb
          (escanear)
          (procesar-tipo-as))
      (= (simb-actual amb) (symbol "."))
      (-> amb
          (escanear)
          (procesar-ident-punto))
      :else amb)
    amb))

(defn procesar-mas-opcional-expresion [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol ","))
      (-> amb
          (escanear)
          (procesar-opcional-expresion)
          (recur))
      amb)
    amb))

(defn procesar-opcional-expresion [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol ")"))
      amb
      (-> amb
          (expresion)
          (procesar-mas-opcional-expresion)))
    amb))

(defn generar-factor-const-o-var [amb puntero?]
  (if (= (estado amb) :sin-errores)
    (let [coincidencias (buscar-coincidencias amb),
          tipo (first (second (last coincidencias))),
          valor (nth (last coincidencias) 2)]
      (if (= tipo 'const)
        (generar amb 'PUSHFI valor)
        (if (not puntero?)
          (generar amb 'PUSHFM valor)
          (generar amb 'PUSHREF valor))))
    amb))

(defn procesar-opcional-asignacion-llamada [amb puntero?]
  (if (= (estado amb) :sin-errores)
    (let [ident (last (simb-ya-parseados amb))]
      (cond
        (= (simb-actual amb) (symbol "="))
        (if (not puntero?)
          (-> amb
              (verificar-que-sea-var)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POP ident))
          (-> amb
              (verificar-que-sea-var-ref)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPREF ident)))
        (= (simb-actual amb) (symbol "+="))
        (if (not puntero?)
          (-> amb
              (verificar-que-sea-var)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPADD ident))
          (-> amb
              (verificar-que-sea-var-ref)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPADDREF ident)))
        (= (simb-actual amb) (symbol "-="))
        (if (not puntero?)
          (-> amb
              (verificar-que-sea-var)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPSUB ident))
          (-> amb
              (verificar-que-sea-var-ref)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPSUBREF ident)))
        (= (simb-actual amb) (symbol "*="))
        (if (not puntero?)
          (-> amb
              (verificar-que-sea-var)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPMUL ident))
          (-> amb
              (verificar-que-sea-var-ref)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPMULREF ident)))
        (= (simb-actual amb) (symbol "/="))
        (if (not puntero?)
          (-> amb
              (verificar-que-sea-var)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPDIV ident))
          (-> amb
              (verificar-que-sea-var-ref)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPDIVREF ident)))
        (= (simb-actual amb) (symbol "%="))
        (if (not puntero?)
          (-> amb
              (verificar-que-sea-var)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPMOD ident))
          (-> amb
              (verificar-que-sea-var-ref)
              (escanear)
              (expresion)
              (generar-con-valor ,,, 'POPMODREF ident)))
        (= (simb-actual amb) (symbol "("))
        (-> amb
            (verificar-que-sea-fn)
            (escanear)
            (procesar-opcional-expresion)
            (procesar-terminal ,,, (symbol ")") 49)
            (generar-con-valor ,,, 'CAL ident))
        :else (-> amb
                  (verificar-que-sea-const-o-var)
                  (generar-factor-const-o-var ,,, puntero?))))
    amb))

(defn expresion-atomica [amb]
  (if (= (estado amb) :sin-errores)
    (cond
      (numero? (simb-actual amb))
      (-> amb
          (generar ,,, 'PUSHFI (simb-actual amb))
          (escanear)
          (procesar-opcional-as))
      (booleano? (simb-actual amb))
      (-> amb
          (generar ,,, 'PUSHFI (simb-actual amb))
          (escanear)
          (procesar-opcional-as))
      (cadena? (simb-actual amb))
      (-> amb
          (generar ,,, 'PUSHFI (simb-actual amb))
          (escanear)
          (procesar-opcional-string-punto))
      (identificador? (simb-actual amb))
      (-> amb
          (escanear)
          (procesar-opcional-asignacion-llamada ,,, false)
          (procesar-opcional-as-punto))
      (= (simb-actual amb) (symbol "("))
      (-> amb
          (escanear)
          (expresion)
          (procesar-terminal ,,, (symbol ")") 12)
          (procesar-opcional-as-punto))
      :else
      (case (simb-actual amb)
        if (let [primera-fase (-> amb
                                  (escanear)
                                  (expresion))]
             (if (= (estado primera-fase) :sin-errores)
               (let [segunda-fase (-> primera-fase
                                      (generar ,,, 'JC (+ 2 (count (bytecode primera-fase))))
                                      (generar ,,, 'JMP '?))]
                 (if (= (estado segunda-fase) :sin-errores)
                   (let [tercera-fase (-> segunda-fase
                                          (inicializar-contexto-local)
                                          (bloque)
                                          (restaurar-contexto-anterior))]
                     (if (= (estado tercera-fase) :sin-errores)
                       (if (= (simb-actual tercera-fase) 'else)
                         (-> tercera-fase
                             (escanear)
                             (generar ,,, 'JMP '?)
                             (inicializar-contexto-local)
                             (fixup ,,, (inc (count (bytecode primera-fase))))
                             (bloque)
                             (fixup ,,, (count (bytecode tercera-fase)))
                             (restaurar-contexto-anterior))
                         (fixup tercera-fase (inc (count (bytecode primera-fase)))))
                       tercera-fase))
                   segunda-fase))
               primera-fase))
        while (let [primera-fase (escanear amb),
                    segunda-fase (expresion primera-fase)]
                (if (= (estado segunda-fase) :sin-errores)
                  (-> segunda-fase
                      (generar ,,, 'JC (+ 2 (count (bytecode segunda-fase))))
                      (generar ,,, 'JMP '?)
                      (inicializar-contexto-local)
                      (bloque)
                      (generar ,,, 'JMP (count (bytecode primera-fase)))
                      (fixup ,,, (inc (count (bytecode segunda-fase))))
                      (restaurar-contexto-anterior))
                  segunda-fase))
        return (-> amb
                   (escanear)
                   (expresion)
                   (generar ,,, 'RET))
        process (-> amb
                    (escanear)
                    (procesar-terminal ,,, (symbol "::") 5)
                    (procesar-terminal ,,, 'exit 22)
                    (procesar-terminal ,,, (symbol "(") 11)
                    (expresion)
                    (procesar-terminal ,,, (symbol ")") 12)
                    (generar ,,, 'HLT))
        format! (-> amb
                    (escanear)
                    (procesar-terminal ,,, (symbol "(") 11)
                    (procesar-terminal ,,, cadena? 25)
                    (generar-pushfi-cadena)
                    (procesar-opcional-expresiones-a-imprimir)
                    (procesar-terminal ,,, (symbol ")") 49)
                    (generar-format!))
        print! (-> amb
                   (escanear)
                   (procesar-terminal ,,, (symbol "(") 11)
                   (procesar-terminal ,,, cadena? 25)
                   (generar-pushfi-cadena)
                   (procesar-opcional-expresiones-a-imprimir)
                   (procesar-terminal ,,, (symbol ")") 49)
                   (generar-print!))
        println! (-> amb
                     (escanear)
                     (procesar-terminal ,,, (symbol "(") 11)
                     (procesar-opcional-cadena-expresiones-a-imprimir)
                     (procesar-terminal ,,, (symbol ")") 49)
                     (generar-print!)
                     (generar ,,, 'NL))
        io (-> amb
               (escanear)
               (procesar-terminal ,,, (symbol "::") 5)
               (procesar-stdout-stdin)
               (procesar-terminal ,,, (symbol ".") 23)
               (procesar-terminal ,,, 'expect 24)
               (procesar-terminal ,,, (symbol "(") 11)
               (procesar-terminal ,,, cadena? 25)
               (procesar-terminal ,,, (symbol ")") 12))
        & (-> amb
              (escanear)
              (procesar-terminal ,,, 'mut 16)
              (procesar-terminal ,,, identificador? 10)
              (verificar-que-sea-var-mut)
              (generar-ref))
        String (-> amb
                   (escanear)
                   (procesar-terminal ,,, (symbol "::") 5)
                   (procesar-string-new-from)
                   (procesar-opcional-string-punto))
        f64 (-> amb
                (escanear)
                (procesar-terminal ,,, (symbol "::") 5)
                (procesar-f64-funcion)
                (procesar-opcional-as))
        * (-> amb
              (escanear)
              (procesar-terminal ,,, identificador? 10)
              (procesar-opcional-asignacion-llamada ,,, true)
              (procesar-opcional-as-punto))
        (dar-error amb 21)))
    amb))

(defn expresion-unaria  [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      + (-> amb
            (escanear)
            (expresion-atomica))
      - (-> amb
            (escanear)
            (expresion-atomica)
            (generar ,,, 'NEG))
      ! (-> amb
            (escanear)
            (expresion-atomica)
            (generar ,,, 'NOT))
      (expresion-atomica amb))
    amb))

(defn procesar-mas-expresion-unaria [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      * (-> amb
            (escanear)
            (expresion-unaria)
            (generar ,,, 'MUL)
            (recur))
      / (-> amb
            (escanear)
            (expresion-unaria)
            (generar ,,, 'DIV)
            (recur))
      % (-> amb
            (escanear)
            (expresion-unaria)
            (generar ,,, 'MOD)
            (recur))
      amb)
    amb))

(defn expresion-multiplicativa [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (expresion-unaria)
        (procesar-mas-expresion-unaria))
    amb))

(defn procesar-mas-expresion-multiplicativa [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      + (-> amb
            (escanear)
            (expresion-multiplicativa)
            (generar ,,, 'ADD)
            (recur))
      - (-> amb
            (escanear)
            (expresion-multiplicativa)
            (generar ,,, 'SUB)
            (recur))
      amb)
    amb))

(defn expresion-aditiva [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (expresion-multiplicativa)
        (procesar-mas-expresion-multiplicativa))
    amb))

(defn procesar-mas-expresion-aditiva [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      <= (-> amb
             (escanear)
             (expresion-aditiva)
             (generar ,,, 'LTE)
             (recur))
      < (-> amb
            (escanear)
            (expresion-aditiva)
            (generar ,,, 'LT)
            (recur))
      >= (-> amb
             (escanear)
             (expresion-aditiva)
             (generar ,,, 'GTE)
             (recur))
      > (-> amb
            (escanear)
            (expresion-aditiva)
            (generar ,,, 'GT)
            (recur))
      amb)
    amb))

(defn expresion-relacional [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (expresion-aditiva)
        (procesar-mas-expresion-aditiva))
    amb))

(defn procesar-mas-expresion-relacional [amb]
  (if (= (estado amb) :sin-errores)
    (case (simb-actual amb)
      != (-> amb
             (escanear)
             (expresion-relacional)
             (generar ,,, 'NEQ)
             (recur))
      == (-> amb
             (escanear)
             (expresion-relacional)
             (generar ,,, 'EQ)
             (recur))
      amb)
    amb))

(defn expresion-igualdad [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (expresion-relacional)
        (procesar-mas-expresion-relacional))
    amb))

(defn procesar-mas-expresion-igualdad [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol "&&"))
      (-> amb
          (escanear)
          (expresion-igualdad)
          (generar ,,, 'AND)
          (recur))
      amb)
    amb))

(defn expresion-and [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (expresion-igualdad)
        (procesar-mas-expresion-igualdad))
    amb))

(defn procesar-mas-expresion-and [amb]
  (if (= (estado amb) :sin-errores)
    (if (= (simb-actual amb) (symbol "||"))
      (-> amb
          (escanear)
          (expresion-and)
          (generar ,,, 'OR)
          (recur))
      amb)
    amb))

(defn expresion [amb]
  (if (= (estado amb) :sin-errores)
    (-> amb
        (expresion-and)
        (procesar-mas-expresion-and))
    amb))

(defn generar-operador-relacional [amb operador]
  (if (= (estado amb) :sin-errores)
    (case operador
      = (generar amb 'EQ)
      <> (generar amb 'NEQ)
      > (generar amb 'GT)
      >= (generar amb 'GTE)
      < (generar amb 'LT)
      <= (generar amb 'LTE))
    amb))

(defn generar-con-valor
  ([amb instr]
   (if (= (estado amb) :sin-errores)
     (let [coincidencias (buscar-coincidencias amb),
           valor (nth (last coincidencias) 2)]
       (generar amb instr valor))
     amb))
  ([amb instr ident]
   (if (= (estado amb) :sin-errores)
     (let [coincidencias (buscar-coincidencias amb ident),
           valor (nth (last coincidencias) 2)]
       (generar amb instr valor))
     amb)))

(defn aplicar-operador-monadico [op pila]
  (try (vec (conj (vec (butlast pila)) (op (last pila))))
       (catch Exception e (print "ERROR: ") (println (buscar-mensaje 55)) nil)))

(defn aplicar-operador-diadico [op pila]
  (try (vec (conj (vec (drop-last 2 pila)) (op (last (butlast pila)) (last pila))))
       (catch Exception e (print "ERROR: ") (println (buscar-mensaje 56)) nil)))

(defn asignar-aritmetico [regs-de-act pila reg-actual fetched op]
  (let [direc (second fetched),
        tipo-en-reg (first (reg-actual direc)),
        dato-en-reg (second (reg-actual direc)),
        dato-en-pila (last pila)]
    (if (compatibles? tipo-en-reg dato-en-pila)
      (cargar-en-ult-reg regs-de-act direc tipo-en-reg (op dato-en-reg dato-en-pila))
      (do (print "ERROR: ") (println (buscar-mensaje 50)) nil))))

(defn asignar-aritmetico-ref [regs-de-act pila reg-actual fetched op]
  (let [direc (second fetched),
        destino (second (reg-actual direc)),
        dato-en-pila (last pila),
        dato-en-dest (second ((regs-de-act (first destino)) (second destino)))
        tipo-en-dest (first ((regs-de-act (first destino)) (second destino)))]
    (if (compatibles? tipo-en-dest dato-en-pila)
      (cargar-en-reg-dest regs-de-act destino tipo-en-dest (op dato-en-dest dato-en-pila))
      (do (print "ERROR: ") (println (buscar-mensaje 50)) nil))))

(defn params-args [cad]
  (let [res (map vec
                 (map reverse
                      (reverse
                       (reduce #(if (neg? (first %1))
                                  (reduced (conj (vec (butlast (second %1))) (vec (butlast (last (second %1))))))
                                  (cond
                                    (= %2 (symbol ")")) [(+ (first %1) 1) (conj (vec (butlast (second %1))) (conj (last (second %1)) %2))]
                                    (= %2 (symbol "(")) [(- (first %1) 1) (conj (vec (butlast (second %1))) (conj (last (second %1)) %2))]
                                    (and (= %2 (symbol ",")) (zero? (first %1))) [(first %1) (conj (second %1) [])]
                                    :else [(first %1) (conj (vec (butlast (second %1))) (conj (last (second %1)) %2))]))
                               [0 [[]]] (rest (reverse cad))))))]
    (if (= res '([])) () res)))

(defn generar-print! [amb]
  (if (= (estado amb) :sin-errores)
    (let [args (params-args (simb-ya-parseados amb))]
      (-> amb
          (generar ,,, 'PUSHFI (count args))
          (generar ,,, 'OUT)))
    amb))

(defn generar-format! [amb]
  (if (= (estado amb) :sin-errores)
    (let [args (params-args (simb-ya-parseados amb))]
      (-> amb
          (generar ,,, 'PUSHFI (count args))
          (generar ,,, 'FMT)))
    amb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LA SIGUIENTE FUNCION DEBERA SER COMPLETADA PARA QUE ANDE EL INTERPRETE DE RUST
; FALTAN IMPLEMENTAR (todas como llamados recursivos a la funcion interpretar, con recur y argumentos actualizados):
;
; PUSHFI: PUSH FROM INSTRUCTION. Direccionamiento inmediato. Incrementa cont-prg en 1 y agrega al final de pila el valor del argumento.
; PUSHFM: PUSH FROM MEMORY. Direccionamiento directo. Incrementa cont-prg en 1 y agrega al final de pila el elemento ubicado en la posicion de reg-actual indicada por el valor del argumento.
; JMP: Salto incondicional. Cambia cont-prg por el valor del argumento.
; JC: Salto condicional. Quita el ultimo valor de la pila. Si este es true, cambia cont-prg por el valor del argumento. Si no, incrementa cont-prg en 1.
; CAL: Llamada a una funcion. Agrega al final de regs-de-act el reg-de-act (proveniente de mapa-regs) indicado por el argumento, cambia cont-prg por el valor del argumento y coloca al final de la pila la direccion de retorno (el valor del argumento incrementado en 1).
; RETN: Indica el retorno de la llamada a un procedimiento (no funcion). Llama recursivamente a interpretar con valores actualizados de regs-de-act (se elimina el ultimo de ellos), cont-prg (pasa a ser el ultimo valor en la pila) y pila (se quita de ella el nuevo cont-prg).
; NL: New line. Imprime un salto de linea e incrementa cont-prg en 1.
; FLUSH: Purga la salida e incrementa cont-prg en 1.
; POPSUB: Como POPADD, pero resta.
; POPMUL: Como POPADD, pero multiplica.
; POPDIV: Como POPADD, pero divide.
; POPMOD: Como POPADD, pero calcula el resto de la division.
; POPSUBREF: Como POPADDREF, pero resta.
; POPMULREF: Como POPADDREF, pero multiplica.
; POPDIVREF: Como POPADDREF, pero divide.
; POPMODREF: Como POPADDREF, pero calcula el resto de la division.
; SUB: Como ADD, pero resta.
; MUL: Como ADD, pero multiplica.
; DIV: Como ADD, pero divide.
; MOD: Como ADD, pero calcula el resto de la division.
; CHR: Incrementa cont-prg en 1, quita de la pila dos elementos (un string y un indice), selecciona el char del string indicado por el indice y lo coloca al final de la pila.
; OR: Como ADD, pero calcula el or entre los dos valores.
; AND: Como ADD, pero calcula el and entre los dos valores.
; EQ: Como ADD, pero calcula la operacion relacional = entre los dos valores.
; NEQ: Como ADD, pero calcula la operacion relacional != entre los dos valores.
; GT:  Como ADD, pero calcula la operacion relacional > entre los dos valores.
; GTE: Como ADD, pero calcula la operacion relacional >= entre los dos valores.
; LT: Como ADD, pero calcula la operacion relacional < entre los dos valores.
; LTE: Como ADD, pero calcula la operacion relacional <= entre los dos valores.
; NEG: Incrementa cont-prg en 1, quita de la pila un elemento numerico, le cambia el signo y lo coloca al final de la pila.
; NOT: Incrementa cont-prg en 1, quita de la pila un elemento booleano, lo niega y lo coloca al final de la pila.
; TOI: Incrementa cont-prg en 1, quita de la pila un elemento numerico, lo convierte a entero y lo coloca al final de la pila.
; TOF: Incrementa cont-prg en 1, quita de la pila un elemento numerico, lo convierte a punto flotante y lo coloca al final de la pila.
; SQRT: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su raiz cuadrada y la coloca al final de la pila.
; SIN: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su seno y lo coloca al final de la pila.
; ATAN: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su arcotangente y la coloca al final de la pila.
; ABS: Incrementa cont-prg en 1, quita de la pila un elemento numerico, calcula su valor absoluto y lo coloca al final de la pila.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn interpretar [cod regs-de-act cont-prg pila mapa-regs]
  (let [fetched (cod cont-prg),
        opcode (if (symbol? fetched) fetched (first fetched)),
        reg-actual (last regs-de-act)]
    (case opcode

          ; Detiene la ejecucion (deja de llamar recursivamente a interpretar)
      HLT nil

          ; Incrementa cont-prg en 1 y agrega al final de pila un valor proveniente de regs-de-act cuyas coordenadas [#reg-act, offset] provienen de reg-actual.
          ; Por ejemplo:
          ; fetched: [PUSHREF 3]
          ; reg-actual: [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 nil] [i64 nil]]
          ;                                         3:^^^^^^^^^^^
          ; destino = [0 4]
          ; regs-de-act: [[[String "5"] [i64 23] [i64 5] [i64 0] [i64 23]] [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 nil] [i64 nil]]]
          ;                                                    4:^^^^^^^^
          ;             0:^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          ; pila recibida: [1 150]
          ; pila al llamar recursivamente a interpretar: [1 150 23]
      PUSHREF (let [destino (second (reg-actual (second fetched)))]
                (recur cod regs-de-act (inc cont-prg) (conj pila (second ((regs-de-act (first destino)) (second destino)))) mapa-regs))

          ; Incrementa cont-prg en 1 y agrega al final de pila unas coordenadas [#reg-act, offset]
          ; Por ejemplo:
          ; fetched: [PUSHADDR 3]
          ; reg-actual: [[String "5"] [i64 23] [i64 5] [i64 0] [i64 0]]
          ; regs-de-act: [[[String "5"] [i64 23] [i64 5] [i64 0] [i64 0]]]
          ;               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ count = 1
          ; pila recibida: [1 23 5]
          ; pila al llamar recursivamente a interpretar: [1 23 5 [0 3]]
      PUSHADDR (recur cod regs-de-act (inc cont-prg) (conj pila [(dec (count regs-de-act)) (second fetched)]) mapa-regs)

          ; Incrementa cont-prg en 1 y quita el ultimo elemento de pila. Si hay un argumento, este indica donde colocar el elemento en el ultimo de los regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Si no lo hay, solo incrementa cont-prg en 1 y quita el elemento de la pila.
          ; Por ejemplo:
          ; fetched: [POP 4]
          ; regs-de-act recibido: [[[String "5"] [i64 23] [i64 5] [i64 0] [i64 23]] [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 nil] [i64 nil]]]
          ; reg-actual: [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 nil] [i64 nil]]
          ; pila recibida: [1 150 5]
          ; pila al llamar recursivamente a interpretar: [1 150]
          ; regs-de-act al llamar recursivamente a interpretar: [[[String "5"] [i64 23] [i64 5] [i64 0] [i64 23]] [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 5] [i64 nil]]]
          ;                                                                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ultimo reg-act
          ;                                                                                                                                               4:^^^^^^^
      POP (if (symbol? fetched)
            (recur cod regs-de-act (inc cont-prg) (vec (butlast pila)) mapa-regs)
            (let [direc (second fetched),
                  tipo-en-reg (first (reg-actual direc)),
                  dato-en-pila (last pila)]
              (if (compatibles? tipo-en-reg dato-en-pila)
                (recur cod (cargar-en-ult-reg regs-de-act direc tipo-en-reg dato-en-pila) (inc cont-prg) (vec (butlast pila)) mapa-regs)
                (do (print "ERROR: ") (println (buscar-mensaje 50)) nil))))

          ; Incrementa cont-prg en 1 y quita el penultimo elemento de pila. El argumento indica donde colocar el elemento en el ultimo de los regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Por ejemplo:
          ; fetched: [POPARG 3]
          ; regs-de-act recibido: [[[String "5"] [i64 23] [i64 5] [i64 0] [i64 0]] [[i64 nil] [i64 nil] [i64 nil] [i64 nil] [i64 nil] [i64 nil]]]
          ; reg-actual: [[i64 nil] [i64 nil] [i64 nil] [i64 nil] [i64 nil] [i64 nil]]
          ; pila recibida: [1 23 5 [0 3] [0 4] 150]
          ; pila al llamar recursivamente a interpretar: [1 23 5 [0 3] 150]
          ; regs-de-act al llamar recursivamente a interpretar: [[[String "5"] [i64 23] [i64 5] [i64 0] [i64 0]] [[i64 nil] [i64 nil] [i64 nil] [i64 [0 4]] [i64 nil] [i64 nil]]]
          ;                                                                                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ultimo reg-act
          ;                                                                                                                                   3:^^^^^^^^^^^
      POPARG (let [direc (second fetched),
                   tipo-en-reg (first (reg-actual direc)),
                   dato-en-pila (last (butlast pila))]
               (if (compatibles? tipo-en-reg dato-en-pila)
                 (recur cod (cargar-en-ult-reg regs-de-act direc tipo-en-reg dato-en-pila) (inc cont-prg) (conj (vec (drop-last 2 pila)) (last pila)) mapa-regs)
                 (do (print "ERROR: ") (println (buscar-mensaje 50)) nil)))

          ; Incrementa cont-prg en 1 y quita el ultimo elemento de pila. El argumento indica en reg-actual las coordenadas [#reg-act, offset] donde colocar el elemento en regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Por ejemplo:
          ; fetched: [POPREF 3]
          ; regs-de-act recibido: [[[String "5"] [i64 23] [i64 5] [i64 0] [i64 0]] [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 nil] [i64 nil]]]
          ; reg-actual: [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 nil] [i64 nil]]
          ;                                         3:^^^^^^^^^^^
          ; pila recibida: [1 150 23]
          ; pila al llamar recursivamente a interpretar: [1 150]
          ; regs-de-act al llamar recursivamente a interpretar: [[[String "5"] [i64 23] [i64 5] [i64 0] [i64 23]] [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 nil] [i64 nil]]]
          ;                                                                                           4:^^^^^^^^
          ;                                                    0:^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      POPREF (let [direc (second fetched),
                   destino (second (reg-actual direc)),
                   dato-en-pila (last pila),
                   tipo-en-dest (first ((regs-de-act (first destino)) (second destino)))]
               (if (compatibles? tipo-en-dest dato-en-pila)
                 (recur cod (cargar-en-reg-dest regs-de-act destino tipo-en-dest dato-en-pila) (inc cont-prg) (vec (butlast pila)) mapa-regs)
                 (do (print "ERROR: ") (println (buscar-mensaje 50)) nil)))

          ; Incrementa cont-prg en 1, lee un string desde el teclado y lo coloca en el ultimo de los regs-de-act al llamar recursivamente a interpretar
          ; Por ejemplo:
          ; fetched: [IN 0]
          ; regs-de-act recibido: [[[String ""] [i64 23] [i64 nil] [i64 nil] [i64 nil]]]
          ; reg-actual: [[String ""] [i64 23] [i64 nil] [i64 nil] [i64 nil]]
          ; regs-de-act al llamar recursivamente a interpretar: [[[String "5"] [i64 23] [i64 nil] [i64 nil] [i64 nil]]]
          ;                                                     0:^^^^^^^^^^^^
      IN (let [entr (read-line)]
           (recur cod (cargar-en-ult-reg regs-de-act (second fetched) 'String entr) (inc cont-prg) pila mapa-regs))

          ; Incrementa cont-prg en 1, quita de la pila el contador de argumentos y los argumentos, y coloca al final de la pila un string con estos ultimos (correctamente formateados)
          ; Por ejemplo:
          ; fetched: FMT
          ; pila recibida: [1 153 "Resto: {}" 9 2]
          ; pila: [1 153 "Resto: 9"]
      FMT (let [cant-args (last pila),
                args (take-last cant-args (butlast pila)),
                res (if (pos? cant-args) (apply format (convertir-formato-impresion args)) "")]
            (recur cod regs-de-act (inc cont-prg) (conj (vec (drop-last (+ cant-args 1) pila)) res) mapa-regs))

          ; Incrementa cont-prg en 1, quita de la pila el contador de argumentos y los argumentos, e imprime estos ultimos (correctamente formateados)
          ; Por ejemplo:
          ; fetched: OUT
          ; pila recibida: [1 153 "Resto: {}" 9 2]
          ; pila: [1 153]
          ; Imprime:
          ;          Resto: 9
      OUT (let [cant-args (last pila),
                args (take-last cant-args (butlast pila))]
            (do (if (pos? cant-args) (apply printf (convertir-formato-impresion args)))
                (recur cod regs-de-act (inc cont-prg) (vec (drop-last (+ cant-args 1) pila)) mapa-regs)))

          ; Indica el retorno de la llamada a una funcion (no procedimiento). Llama recursivamente a interpretar con valores actualizados de regs-de-act (se elimina el ultimo de ellos), cont-prg (pasa a ser el penultimo valor en la pila) y pila (se quita de ella el nuevo cont-prg).
          ; Por ejemplo:
          ; fetched: RET
          ; regs-de-act recibido: [[[String "15"] [i64 12] [i64 15]] [[i64 3] [i64 3]]]
          ; cont-prg recibido: 40
          ; pila recibida: [1 "{} es el MCD entre " 81 3]
          ; regs-de-act al llamar recursivamente a interpretar: [[[String "15"] [i64 12] [i64 15]]]
          ; cont-prg al llamar recursivamente a interpretar: 81
          ; pila al llamar recursivamente a interpretar: [1 "{} es el MCD entre " 3]
      RET (recur cod (vec (butlast regs-de-act)) (last (butlast pila)) (vec (conj (vec (drop-last 2 pila)) (last pila))) mapa-regs)


          ; Incrementa cont-prg en 1 y quita el ultimo elemento de pila. El argumento indica donde sumar el elemento en el ultimo de los regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Por ejemplo:
          ; fetched: [POPADD 2]
          ; regs-de-act recibido: [[[String "6"] [i64 6] [i64 5]]]
          ; reg-actual: [[String "6"] [i64 6] [i64 5]]
          ; pila recibida: [1 2]
          ; pila al llamar recursivamente a interpretar: [1]
          ; regs-de-act al llamar recursivamente a interpretar: [[[String "6"] [i64 6] [i64 7]]]
          ;                                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ultimo reg-act
          ;                                                                          2:^^^^^^^
      POPADD (let [res (asignar-aritmetico regs-de-act pila reg-actual fetched +)]
               (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; Incrementa cont-prg en 1 y quita el ultimo elemento de pila. El argumento indica en reg-actual las coordenadas [#reg-act, offset] donde sumar el elemento en regs-de-act al llamar recursivamente a interpretar (verificando la compatibilidad de los tipos)
          ; Por ejemplo:
          ; fetched: [POPADDREF 2]
          ; regs-de-act recibido: [[[String "5"] [i64 23] [i64 5] [i64 0] [i64 3]] [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 5] [i64 20]]]
          ; reg-actual: [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 5] [i64 20]]
          ;                             2:^^^^^^^^^^^
          ; pila recibida: [1 150 1]
          ; pila al llamar recursivamente a interpretar: [1 150]
          ; regs-de-act al llamar recursivamente a interpretar: [[[String "5"] [i64 23] [i64 5] [i64 1] [i64 3]] [[i64 23] [i64 5] [i64 [0 3]] [i64 [0 4]] [i64 5] [i64 20]]]
          ;                                                                                   3:^^^^^^^
          ;                                                    0:^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      POPADDREF (let [res (asignar-aritmetico-ref regs-de-act pila reg-actual fetched +)]
                  (if (nil? res) res (recur cod res (inc cont-prg) (vec (butlast pila)) mapa-regs)))

          ; Incrementa cont-prg en 1, quita de la pila dos elementos, calcula su suma y la coloca al final de la pila
          ; fetched: ADD
          ; pila recibida: [1 0 0 3 4]
          ; pila al llamar recursivamente a interpretar: [1 0 0 7]
      ADD (let [res (aplicar-operador-diadico + pila)]
            (if (nil? res) res (recur cod regs-de-act (inc cont-prg) res mapa-regs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LAS FUNCIONES QUE SIGUEN DEBERAN SER IMPLEMENTADAS PARA QUE ANDE EL INTERPRETE DE RUST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LISTAR: Recibe una lista con los tokens de un programa en Rust (o de parte de un programa) y muestra el codigo fuente formateado. Retorna nil.
; Por ejemplo:
; user=> (listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}")))
; fn main ( )
; {
;   println! ( "Hola, mundo!" )
; }
;
; nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: fixear caso de < > = después de paréntesis. Qué pasa con <=?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn imprimir-indentacion [indentacion]
  (print (apply str (repeat indentacion "  "))))

(defn imprimir-token [indentacion lista-tokens]
  (let [token (first lista-tokens),
        siguiente-token (second lista-tokens),
        resto-tokens (rest lista-tokens),
        tokens-sin-salto-post-parentesis (hash-set (symbol ";") (symbol ".") (symbol "->"))]
    (cond
      (empty? lista-tokens) nil
      :else
        (cond
          (= token (symbol "{")) (do (prn) (imprimir-indentacion indentacion) (print token) (prn) (imprimir-indentacion (inc indentacion)) (recur (inc indentacion) resto-tokens))
          (= token (symbol "}")) (do (prn) (imprimir-indentacion (dec indentacion)) (print token) (if (not (= siguiente-token (symbol "}"))) (do (prn) (imprimir-indentacion (dec indentacion)))) (recur (dec indentacion) resto-tokens))
          (or (= token (symbol ";")) (and (= token (symbol ")")) (not (contains? tokens-sin-salto-post-parentesis siguiente-token)))) (do (print token) (if (or (= siguiente-token (symbol "{")) (= siguiente-token (symbol "}"))) nil (do (prn) (imprimir-indentacion indentacion))) (recur indentacion resto-tokens))
          (string? token) (do (pr token) (print " ") (recur indentacion resto-tokens))
          :else (do (print token) (print " ") (recur indentacion resto-tokens))))))

(defn listar [lista-tokens]
  (do (imprimir-token 0 lista-tokens) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AGREGAR-PTOCOMA: Recibe una lista con los tokens de un programa en Rust y la devuelve con un token ; insertado a continuacion de ciertas } (llaves de cierre, pero no a continuacion de todas ellas).
; Esto se debe a que los programas correctos de Rust no separan con ; las expresiones presentes dentro de un bloque cuando estas terminan en }.
; Por ejemplo:
; user=> (agregar-ptocoma (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}")))
; (fn main ( ) { if x < 0 { x = - x ; } ; renglon = x ; if z < 0 { z = - z ; } } fn foo ( ) { if y > 0 { y = - y ; } else { x = - y ; } })
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _agregar-ptocoma [lista-tokens resultado pila-expresiones-con-bloque]
  (let [token (first lista-tokens),
        siguiente-token (second lista-tokens),
        resto-tokens (rest lista-tokens),
        expresiones-con-bloque (hash-set 'while 'if 'fn 'else)]
    (cond
      (empty? lista-tokens) (apply list resultado)
      (contains? expresiones-con-bloque token) (recur resto-tokens (conj resultado token) (conj pila-expresiones-con-bloque token))
      (= token (symbol "}")) (cond
                                (and (or (= (peek pila-expresiones-con-bloque) 'else) (and (= (peek pila-expresiones-con-bloque) 'if) (not (= siguiente-token 'else)))) (not (= siguiente-token (symbol "}")))) (recur resto-tokens (-> resultado (conj token) (conj (symbol ";"))) (pop pila-expresiones-con-bloque))
                                :else (recur resto-tokens (conj resultado token) (pop pila-expresiones-con-bloque)))
      :else (recur resto-tokens (conj resultado token) pila-expresiones-con-bloque))))

(defn agregar-ptocoma [lista-tokens]
  (_agregar-ptocoma lista-tokens [] []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PALABRA-RESERVADA?: Recibe un elemento y devuelve true si es una palabra reservada de Rust; si no, false.
; Por ejemplo:
; user=> (palabra-reservada? 'while)
; true
; user=> (palabra-reservada? 'until)
; false
; user=> (palabra-reservada? 13)
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; usar contains. Buscar en los 10 ejemplos que nos dio!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn palabra-reservada? [elem]
  (let [palabras-reservadas (hash-set 'use 'const 'fn 'std 'i64 'f64 'mut 'bool 'String 'let 'char 'if 'else 'while 'return 'process 'exit 'format! 'print! 'println! 'io 'as 'usize 'expect 'unwrap true false)]
    (contains? palabras-reservadas elem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IDENTIFICADOR?: Recibe un elemento y devuelve true si es un identificador valido en Rust; si no, false.
; Por ejemplo:
; user=> (identificador? 'boolean)
; true
; user=> (identificador? 'bool)
; false
; user=> (identificador? 'e120)
; true
; user=> (identificador? '12e0)
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; analizar caracter por caracter y comprobar naturaleza

;; cómo tienen que estar formados los identificadores? buscar en los 10 ejemplos que nos dio!

;; NO se pueden usar palabras reservadas. Ej: as, i64 (pero si i65) usar palabra-reservada? !!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn identificador? [elem]
    (not
      (or
        (palabra-reservada? elem)
        (number? elem)
        (not (every? #(or (Character/isLetterOrDigit %) (= \_ %)) (name elem)))
        ;; TODO: tengo que verificar strings, chars, etc? saque (symbol? elem) con el and
        ;; TODO: falta chequear que no sea un número el primer caracter?
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DUMP: Recibe un vector con instrucciones de la RI y las imprime numeradas a partir de 0. Siempre devuelve nil.
; Por ejemplo:
; user=> (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG])
; 0 [POPREF 2]
; 1 [PUSHFI 2]
; 2 MUL
; 3 [PUSHFI 1]
; 4 ADD
; 5 NEG
; nil
; user=> (dump '[HLT])
; 0 HLT
; nil
; user=> (dump nil)
; 0 nil
; nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dump [instrucciones]
  (do
    (doall (map-indexed #(do (print %1) (print " ") (println %2)) (if (nil? instrucciones) '[nil] instrucciones)))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; YA-DECLARADO-LOCALMENTE?: Recibe un identificador y un contexto (un vector formado por dos subvectores: el primero
; con las sucesivas posiciones de inicio de los distintos ambitos/scopes y el segundo con ternas
; [identificador, tipo, valor] resultantes de las declaraciones efectuadas, y devuelve true si el identificador esta
; declarado en el segundo subvector a partir de la ultima posicion guardada en el primer subvector (o sea, en el
; ambito/scope local); si no, devuelve false.
; Por ejemplo:
; user=> (ya-declarado-localmente? 'Write [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
; true
; user=> (ya-declarado-localmente? 'Read [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
; false
; user=> (ya-declarado-localmente? 'Write [[0 1] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
; true
; user=> (ya-declarado-localmente? 'Write [[0 2] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]])
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ya-declarado-localmente? [id ctx]
  (let [ids-scope-local (map first (subvec (second ctx) (last (first ctx))))]
    (if (some #(= id %) ids-scope-local) true false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CARGAR-CONST-EN-TABLA: Recibe un ambiente
; [simb-actual  simb-no-parseados-aun  simb-ya-parseados  estado  contexto  prox-var  bytecode mapa-regs-de-act]
; Si su estado no es :sin-errores, lo devuelve intacto. De lo contrario, lo devuelve modificado con la constante
; declarada como terna [identificador, tipo, valor] en el segundo subvector del vector contexto.
; Por ejemplo:
; user=> (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
; [; (fn main ( ) { println! ( "{}" , TRES ) }) [use std :: io ; const TRES : i64 = 3] 8 [[0] [[io [lib ()] 0]]] 0 [[CAL 0] HLT] []]
;                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^ ^^^^^^^^^^^^^^^^^^^^^^^
; user=> (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
; [; (fn main ( ) { println! ( "{}" , TRES ) }) [use std :: io ; const TRES : i64 = 3] :sin-errores [[0] [[io [lib ()] 0] [TRES [const i64] 3]]] 0 [[CAL 0] HLT] []]
;                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn modificar-amb-si-no-hay-error [cb amb & opts]
  (cond
      (= (estado amb) :sin-errores) (apply cb amb opts)
      :else amb
    )
)

(defn _cargar-const-en-tabla [amb]
  (let [contexto (contexto amb),
        tabla (second contexto),
        simb-ya-parseados-al-reves (reverse (simb-ya-parseados amb))]
    (assoc amb 4 (assoc contexto 1 (conj tabla [(nth simb-ya-parseados-al-reves 4) [(nth simb-ya-parseados-al-reves 5) (nth simb-ya-parseados-al-reves 2)] (first simb-ya-parseados-al-reves)])))))


(defn cargar-const-en-tabla [amb]
    (modificar-amb-si-no-hay-error _cargar-const-en-tabla amb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INICIALIZAR-CONTEXTO-LOCAL: Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto.
; De lo contrario, lo devuelve modificado con el tamano del segundo subvector del vector contexto agregado al final
; del primer subvector del vector contexto.
; Por ejemplo:
; user=> (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
; [{ (let x : i64 = 10 ; println! ( "{}" , x ) }) [fn main ( )] 8 [[0] [[main [fn [() ()]] 2]]] 0 [[CAL 2] HLT] []]
;                                                               ^
; user=> (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
; [{ (let x : i64 = 10 ; println! ( "{}" , x ) }) [fn main ( )] :sin-errores [[0 1] [[main [fn [() ()]] 2]]] 0 [[CAL 2] HLT] []]
;                                                               ^^^^^^^^^^^^     ^  ^^^^^^^^^^^^^^^^^^^^^^^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _inicializar-contexto-local [amb]
  (let [contexto (contexto amb)]
      (assoc amb 4 (assoc contexto 0 (conj (first contexto) (count (second contexto)))))))


(defn inicializar-contexto-local [amb]
  (modificar-amb-si-no-hay-error _inicializar-contexto-local amb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESTAURAR-CONTEXTO-ANTERIOR: Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto.
; De lo contrario, lo devuelve modificado, quitando la ultima frontera en el primer subvector del vector contexto,
; y sacando del final del segundo subvector del vector contexto los elementos ubicados a partir de la frontera quitada.
; Por ejemplo:
; user=> (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
; [EOF () [fn main ( ) { let x : i64 = 10 ; let y : i64 = 20 ; println! ( "{}" , x + y ) }] 8 [[0 1] [[main [fn [() ()]] 2] [x [var-inmut i64] 0] [y [var-inmut i64] 1]]] 2 [[CAL 2] HLT [PUSHFI 10] [POP 0] [PUSHFI 20] [POP 1] [PUSHFI "{}"] [PUSHFM 0] [PUSHFM 1] ADD [PUSHFI 2] OUT NL] [[2 [i64 nil] [i64 nil]]]]
;                                                                                           ^
; user=> (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
; [EOF () [fn main ( ) { let x : i64 = 10 ; let y : i64 = 20 ; println! ( "{}" , x + y ) }] :sin-errores [[0] [[main [fn [() ()]] 2]]] 2 [[CAL 2] HLT [PUSHFI 10] [POP 0] [PUSHFI 20] [POP 1] [PUSHFI "{}"] [PUSHFM 0] [PUSHFM 1] ADD [PUSHFI 2] OUT NL] [[2 [i64 nil] [i64 nil]]]]
;                                                                                           ^^^^^^^^^^^^  ^^^ ^^^^^^^^^^^^^^^^^^^^^^^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _restaurar-contexto-anterior [amb]
  (let [contexto (contexto amb)
        frontera (peek (first contexto))]
    (assoc amb 4 (conj [(pop (first contexto))] (subvec (second contexto) 0 frontera)))))

(defn restaurar-contexto-anterior [amb]
  (modificar-amb-si-no-hay-error _restaurar-contexto-anterior amb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BUSCAR-TIPO-DE-RETORNO: Recibe un ambiente y la direccion de una funcion a ser buscada en el segundo subvector del
; vector contexto. Si la encuentra, devuelve el tipo de dato de retorno de la funcion o la lista vacia si se trata de
; un procedimiento. Si no, devuelve nil.
; Por ejemplo:
; user=> (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 2)
; i64                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ^^^   ^                                                                                                                                                                          ^
; user=> (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 8)
; ()                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ^^   ^                                                                                                                                                  ^
; user=> (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 1)
; nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deducir-tipo-retorno [funcion]
  (let [tipo-retorno (last (last funcion))]
    (cond
      (or (symbol? tipo-retorno) (= '() tipo-retorno)) tipo-retorno
      :else nil
    )
  )
)

(defn procesar-funciones [funciones dir]
  (let [declaracion (reverse (peek funciones))]
    (cond
      (empty? funciones) nil
      (= (first declaracion) dir) (let [tipo-retorno (deducir-tipo-retorno (second declaracion))] (if (not (nil? tipo-retorno)) tipo-retorno (procesar-funciones (pop funciones) dir)))
      :else (recur (pop funciones) dir)
    )))

(defn buscar-tipo-de-retorno [amb dir]
  (let [contexto (contexto amb)]
      (procesar-funciones (second contexto) dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GENERAR-REF: Recibe un ambiente y, si su estado no es :sin-errores, lo devuelve intacto.
; De lo contrario, lo devuelve modificado, generando (llamando a generar) una instruccion PUSHADDR con la direccion
; de la ultima variable proveniente de simb-ya-parseados. Esta direccion sera la de la ultima coincidencia de la
; variable anterior en el segundo subvector del vector contexto.
; Por ejemplo:
; user=> (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
; [) (; println! ( "{}" , v ) ; }) [fn inc ( v : & mut i64 ) { * v += 1 ; } fn main ( ) { let mut v : i64 = 5 ; inc ( & mut v] 8 [[0 2] [[inc [fn [([v : & mut i64]) ()]] 2] [main [fn [() ()]] 6] [v [var-mut i64] 0]]] 1 [[CAL 6] HLT [POPARG 0] [PUSHFI 1] [POPADDREF 0] RETN [PUSHFI 5] [POP 0]] [[2 [i64 nil]] [6 [i64 nil]]]]
;                                                                                                                              ^
; user=> (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
; [) (; println! ( "{}" , v ) ; }) [fn inc ( v : & mut i64 ) { * v += 1 ; } fn main ( ) { let mut v : i64 = 5 ; inc ( & mut v] :sin-errores [[0 2] [[inc [fn [([v : & mut i64]) ()]] 2] [main [fn [() ()]] 6] [v [var-mut i64] 0]]] 1 [[CAL 6] HLT [POPARG 0] [PUSHFI 1] [POPADDREF 0] RETN [PUSHFI 5] [POP 0] [PUSHADDR 0]] [[2 [i64 nil]] [6 [i64 nil]]]]
;                                                                                                                           ^  ^^^^^^^^^^^^                                                                    ^^^^^^^^^^^^^^^^^                                                                               ^^^^^^^^^^^^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn buscar-valor-variable [nombre variables]
  (let [actual (peek variables)]
    (cond
      (empty? variables) nil
      (= (first actual) nombre) (last actual)
      :else (recur nombre (pop variables))
    )))

(defn _generar-ref [amb]
  (let [nombre (last (simb-ya-parseados amb)),
        contexto (contexto amb),
        valor (buscar-valor-variable nombre (reverse (second contexto)))]
    (cond
      (nil? valor) amb
      :else (generar amb 'PUSHADDR valor)
    )))

(defn generar-ref [amb]
  (modificar-amb-si-no-hay-error _generar-ref amb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FIXUP: Recibe un ambiente y la ubicacion de un JMP ? a corregir en el vector de bytecode. Si el estado no es
; :sin-errores, devuelve el ambiente intacto. De lo contrario, lo devuelve con el JMP corregido con el tamano del
; vector de bytecode.
; Por ejemplo:
; user=> (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
; [{ (x = 20 ; } ; println! ( "{}" , x ) }) [fn main ( ) { let x : i64 ; if false { x = 10 ; } else] 8 [[0 1 2] [[main [fn [() ()]] 2] [x [var-inmut i64] 0]]] 1 [[CAL 2] HLT [PUSHFI false] [JC 5] [JMP ?] [PUSHFI 10] [POP 0] [JMP ?]] [[2 [i64 nil]]]]
;                                                                                                    ^
; user=> (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
; [{ (x = 20 ; } ; println! ( "{}" , x ) }) [fn main ( ) { let x : i64 ; if false { x = 10 ; } else] :sin-errores [[0 1 2] [[main [fn [() ()]] 2] [x [var-inmut i64] 0]]] 1 [[CAL 2] HLT [PUSHFI false] [JC 5] [JMP 8] [PUSHFI 10] [POP 0] [JMP ?]] [[2 [i64 nil]]]]
;                                                                                                    ^^^^^^^^^^^^                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^: tamano 8                                                                                                                                                                                                                                        ^ ubicacion de JMP ? en contexto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _fixup [amb jump-idx]
  (let [bytecode (bytecode amb)]
    (assoc amb 6 (assoc bytecode jump-idx [(symbol "JMP") (count bytecode)]))))

(defn fixup [amb jump-idx]
  (modificar-amb-si-no-hay-error _fixup amb jump-idx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CONVERTIR-FORMATO-IMPRESION: Recibe una lista con los argumentos de print! de Rust y devuelve una lista con los
; argumentos equivalentes de printf de Clojure.
; Por ejemplo:
; user=> (convertir-formato-impresion '("Hola, mundo!"))
; ("Hola, mundo!")
; user=> (convertir-formato-impresion '("- My name is {}, James {}.\n- Hello, {}{}{}!" "Bond" "Bond" 0 0 7))
; ("- My name is %s, James %s.\n- Hello, %d%d%d!" "Bond" "Bond" 0 0 7)
; user=> (convertir-formato-impresion '("{} elevado a la {} es\t{}" 2.0 2 4.0))
; ("%.0f elevado a la %d es\t%.0f" 2.0 2 4.0)
; user=> (convertir-formato-impresion '("Las raices cuadradas de {} son +{:.8} y -{:.8}" 4.0 1.999999999985448 1.999999999985448))
; ("Las raices cuadradas de %.0f son +%.8f y -%.8f" 4.0 1.999999999985448 1.999999999985448)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn determinar-placeholder [arg match]
  (let [digitos-float (second match)]
    (cond
      (not (nil? digitos-float)) (str "%." digitos-float "f")
      (string? arg) "%s"
      (float? arg) "%.0f"
      (integer? arg) "%d"
    )
  )
)

(defn _convertir-formato-impresion [args-por-procesar args-ya-procesados cadena]
  (let [arg (first args-por-procesar)]
    (cond
      (nil? arg) (cons cadena (reverse args-ya-procesados))
      :else (recur (pop args-por-procesar) (cons arg args-ya-procesados) (clojure.string/replace-first cadena #"\{\}|\{:.(\d)\}" (partial determinar-placeholder arg))))
  )
)

(defn convertir-formato-impresion [lista]
  (_convertir-formato-impresion (pop lista) '() (first lista)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DIVIDIR: Recibe dos numeros y devuelve su cociente, manteniendo su tipo.
; Por ejemplo:
; user=> (dividir 12 3)
; 4
; user=> (dividir 12.0 3)
; 4.0
; user=> (dividir 12 3.0)
; 4.0
; user=> (dividir 12.0 3.0)
; 4.0
; user=> (dividir 1 2)
; 0
; user=> (dividir 1 2.0)
; 0.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dividir [a b]
  (cond
    (and (integer? a) (integer? b)) (quot a b)
    :else (/ a b)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMPATIBLES?: Recibe dos elementos. Si el primero es un tipo de dato de Rust y el segundo es un valor de Clojure
; de un tipo de dato compatible con el mismo o un vector, devuelve true. Si no, false.
; Por ejemplo:
; user=> (compatibles? 'i64 5)
; true
; user=> (compatibles? 'i64 5.0)
; false
; user=> (compatibles? 'i64 [5.0])
; true
; user=> (compatibles? 'f64 5.0)
; true
; user=> (compatibles? 'String "Hola")
; true
; user=> (compatibles? 'bool true)
; true
; user=> (compatibles? 'bool 1)
; false
; user=> (compatibles? 'usize 1)
; true
; user=> (compatibles? 'char \a)
; true
; user=> (compatibles? 'char 'a)
; false
; user=> (compatibles? 'char ['a])
; true
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compatibles? [tipo valor]
  (or
    (vector? valor)
    (and (= 'i64 tipo) (integer? valor))
    (and (= 'f64 tipo) (float? valor))
    (and (= 'String tipo) (string? valor))
    (and (= 'bool tipo) (boolean? valor))
    (and (= 'usize tipo) (pos-int? valor))
    (and (= 'char tipo) (char? valor))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PASAR-A-INT: Recibe un elemento. Si puede devolverlo expresado como un entero, lo hace. Si no, lo devuelve intacto.
; Por ejemplo:
; user=> (pasar-a-int "10")
; 10
; user=> (pasar-a-int 10.0)
; 10
; user=> (pasar-a-int 10)
; 10
; user=> (pasar-a-int 'a)
; a
; user=> (pasar-a-int [10.0])
; [10.0]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pasar-a-int [elem]
  (try (if (string? elem) (Integer/parseInt elem) (int elem))
  (catch ClassCastException e elem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PASAR-A-FLOAT: Recibe un elemento. Si puede devolverlo expresado como un numero de punto flotante, lo hace. Si no,
; lo devuelve intacto.
; Por ejemplo:
; user=> (pasar-a-float "10")
; 10.0
; user=> (pasar-a-float 10)
; 10.0
; user=> (pasar-a-float 10.0)
; 10.0
; user=> (pasar-a-float 'a)
; a
; user=> (pasar-a-float [10])
; [10]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pasar-a-float [elem]
  (try (if (string? elem) (Float/parseFloat elem) (float elem))
  (catch ClassCastException e elem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CARGAR-EN-ULT-REG: Recibe un vector de registros de activacion, una direccion, un tipo y un valor. Devuelve el
; vector de registros de activacion con el ultimo registro actualizado, en la direccion indicada, con el nuevo tipo
; y el nuevo valor.
; Por ejemplo:
; user=> (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 nil]]] 1 'i64 0)
; [[[String "2"] [i64 6] [i64 2] [i64 3] [i64 0]] [[i64 nil] [i64 0]]]
;                                                             ^^^ ^
; user=> (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]] 0 'f64 3)
; [[[String "2"] [i64 6] [i64 2] [i64 3] [i64 0]] [[f64 3] [i64 0]]]
;                                                   ^^^ ^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn cargar-en-ult-reg [regs-de-act idx tipo valor]
;;   (let [registro-a-cargar (last regs-de-act)
;;         idx-registro-a-cargar (dec (count regs-de-act))]
;;     (assoc regs-de-act idx-registro-a-cargar (assoc registro-a-cargar idx [tipo valor]))
;;   ))

(defn cargar-en-ult-reg [regs-de-act idx tipo valor]
    (cargar-en-reg-dest regs-de-act [(dec (count regs-de-act)) idx] tipo valor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CARGAR-EN-REG-DEST: Recibe un vector de registros de activacion, coordenadas, un tipo y un valor. Devuelve el
; vector de registros de activacion con el registro indicado por las coordenadas actualizado, con el nuevo tipo
; y el nuevo valor.
;
; Por ejemplo:
; user=> (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 2]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 4] 'i64 0)
; [[[String "2"] [i64 6] [i64 2] [i64 2] [i64 0]] [[i64 6] [i64 2] [i64 [0 3]] [i64 [0 4]] [i64 2] [i64 2]]]
;                                         ^^^ ^
; user=> (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 3] 'f64 3)
; [[[String "2"] [i64 6] [i64 2] [f64 3] [i64 0]] [[i64 6] [i64 2] [i64 [0 3]] [i64 [0 4]] [i64 2] [i64 2]]]
;                                 ^^^ ^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cargar-en-reg-dest [regs-de-act coordenadas tipo valor]
  (let [idx-registro-a-cargar (first coordenadas),
        registro-a-cargar (nth regs-de-act idx-registro-a-cargar),
        idx-dentro-registro (second coordenadas)]
    (assoc regs-de-act idx-registro-a-cargar (assoc registro-a-cargar idx-dentro-registro [tipo valor]))
  ))

true