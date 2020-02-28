(ns b3djs.parser
  (:require [b3djs.toker :as toker]))

(def empty-env {:symbols {} :parent nil})
(def empty-program {})

(def ^:dynamic *env* empty-env)

(defn add-symbol [sym details]
  (set! *env* (assoc-in *env* [:symbols sym] details)))

(defn push-scope []
  (assoc empty-env :parent *env*))

;; Symbol types
;; - variables
;; - functions
;; - structs

(defn function []
  ;;read name + signature - add this to scope
  ;;push new scope,
  ;;  add variables to scope
  ;;  analyse statements
)

(defn statement-def []
  ;;read name + type - add to this scope
  ;;if there is an assignment, eval expression
  )
(defn statement-for []
  )

(defn function-def [lexer]
  )

(defn type-def [lexer]
  )

(defn param-list-items [lexer]
  ;;Repeat while :comma after param
  [[{} {}] lexer])

(defn param-list [lexer]
  (let [lexer (toker/drop-if lexer :oparen)
        [items lexer] (param-list-items lexer)
        lexer (toker/drop-if lexer :lparen)
        lexer (toker/drop-required lexer :newline)]
    [items lexer]))

(defn function-call [lexer]
  (let [[ident lexer] (toker/consume lexer)
        [param-list lexer] (param-list lexer)]
    [{:type :function-call
      :ident ident
      :params param-list} lexer]))

(defn field-assignment [lexer]
  (print "Field assignment"))

(defn variable-assignment [lexer]
  (print "Variable assignment"))

(defn statement [lexer]
  (case (-> lexer toker/current-token :type)
    :newline (statement (toker/drop-token lexer)) ;;TODO: Make recur?
    :ident (case (-> lexer toker/next-token :type)
             :eq (variable-assignment lexer)
             :field-access (field-assignment lexer)
             (function-call lexer)))
  ;;def-var
  ;;for
  ;;if
  ;;
  )

(defn program-statement [lexer]
  (case (-> lexer toker/current-token :type)
    :function (function-def lexer)
    :type (type-def lexer)
    (statement lexer)))

(defn program [lexer]
  ;;loop while not EOF
  ;;statement-
  ;;function-def

  ;; Repeat until EOF

  (let [[statements lexer] (program-statement lexer)]
    [{:type :program
      :statements statements} lexer]))

(defn build-ast [lexer]
  (binding [*env* (push-scope)]
    ;;HACK: To prime the lexer
    (let [[_ lexer] (toker/consume lexer)
          [program _lexer] (program lexer)]
      program))) 

#_(defn change-z []
    (print *env*)
    (add-symbol "x" {:value 24})
    (add-symbol "y" {:type :fn :sig [{:id "name" :type "int"}]})
    (print *env*))


#_(do
  (print *env*)
  (binding [*env* (push-scope)]
    (print *env*)
    (change-z)
    (print *env*))
  (print *env*)
  )