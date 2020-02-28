(ns b3djs.toker
  (:require [clojure.string :as s]
            [goog.string :as gstring]))

(def compiler-state 
  {:col 0
   :line 0
   :source nil
   :source-lines nil
   :current-line nil
   
   :current nil
   :next nil
   })

(defn- alpha? [str]
  (gstring/isAlpha str))

(defn- alpha-numeric? [str]
  (gstring/isAlphaNumeric str))

(defn- numeric? [str]
  (gstring/isNumeric str))

(defn- valid-ident-char? [char]
  (or (= char "_") 
      (alpha-numeric? char)))

(def simple-tokens 
  {"\\" :field-access
   "(" :oparen
   ")" :lparen
   "," :comma
   "." :period
   "+" :add
   "-" :sub
   "/" :div
   "*" :mul
   ">" :gt
   "<" :lt
   "=" :eq})

(def escape-codes
  {"n" "\n"
   "\\" "\\"
   "\"" "\""
   "t" "\t"})

(def keywords 
  {"function" :function
   "endfunction" :function-end
   "type" :type
   "endtype" :type-end
   "for" :for
   "to" :to
   })

(defn- next-line [state]
  ;;Inc row
  (-> state
      (assoc :current-line (first (:source-lines state)))
      (assoc :source-lines (rest (:source-lines state)))))

(defn- eof? [state] (empty? (:source-lines state)))

(defn- rest-if [sequence pred]
  (if (pred (first sequence)) (rest sequence) sequence))

(defn read-ident [line] 
  (let [[ident remain] (split-with valid-ident-char? line)
        ;; stringify and normalize identifer
        ident (->> ident 
                   (apply str) 
                   (s/lower-case))
        
        ;; Remove postfix "type hints" from the end of identifiers if present
        remain (-> remain
                   (rest-if #(= % "#"))
                   (rest-if #(= % "$")))]
    (if-let [keyword (get keywords ident)]
      [{:type keyword} remain]
      [{:type :ident :value ident} remain])))

(defn read-string [line] 
  (loop [remain (rest line) ;;Drop the first "
         string ""]

    (let [[c n] (take 2 remain)
          remain (rest remain)]
      (cond
        (nil? c) [{:type :string :value string :err "Missing \""} remain ] ;;Add error here, unterminated string
        (= "\"" c) [{:type :string :value string} remain]
        
        (= "\\" c) (if-let [code (get escape-codes n)]
                     (recur (rest remain) (str string code))
                     (recur (rest remain) string)) ;;Drop invalid code TODO: Add error
        
        :else (recur remain (str string c))))))

(defn read-binary [str] [{:type :binary} (drop 1 str)])
(defn read-hex [str] [{:type :hex} (drop 1 str)])

(defn read-number 
  ;;TODO: Improve this, regex? take enough + then parse?
  "The worlds worst number parser"
  [line]
  (let [read-neg (fn [[n line]]
                   (if (= "-" (first line))
                     [(assoc n :neg true) (rest line)]
                     [(assoc n :neg false) line]))
        read-digits (fn [key]
                      (fn [[n line]]
                        (let [[num line] (split-with numeric? line)
                              num (apply str num)]
                          [(assoc n key num) line])))
        read-f (read-digits :f)
        read-s (read-digits :s)
        read-point (fn [[n line]]
                     (if (= "." (first line))
                       [(assoc n :type :float) (rest line)]
                       [(assoc n :type :int) line]))
        to-number (fn [[n line]]
                    (let [neg-prefix (if (:neg n) "-" "")
                          value (if (= (:type n) :int)
                                  (js/parseInt (str neg-prefix (:f n)))
                                  (js/parseFloat (str neg-prefix (:f n) "." (:s n))))]
                      [(assoc n :value value) line]))]
    (-> [{} line]
        (read-neg)
        (read-f)
        (read-point)
        (read-s)
        (to-number))))

(defn parse-token 
  "Returns a map containing information about the token
   and the remainder of the line"
  [line]
  (let [[c n1 n2] (take 3 line)]
    (cond
      (nil? c) nil

      (s/blank? c) [nil (drop 1 line)]
      (= c ";") [nil ""]

      (and (= c ".") (numeric? n1)) (read-number line)
      (and (= c "-") (= n1 ".") (numeric? n2)) (read-number line)
      (or (numeric? c)
          (and (= c '-') (numeric? n1))) (read-number line)
      
      (= c "$") (read-hex line)
      (= c "%") (read-binary line)
      (= c "\"") (read-string line)
      (alpha? c) (read-ident line)

      (and (= c "<") (= n1 "=")) [{:type :lte} (drop 2 line)]
      (and (= c ">") (= n1 "=")) [{:type :gte} (drop 2 line)]
      (and (= c "<") (= n1 ">")) [{:type :ne} (drop 2 line)]

      (simple-tokens c) [{:type (simple-tokens c)} (drop 1 line)]
      :else [{:type :unknown} (drop 1 line)])))

(defn line->tokens [line]
  (loop [remain line
         tokens []]
    
    (if-let [[token res] (parse-token remain)]
      (recur res (if token (conj tokens token) tokens))
      (conj tokens {:type :newline}))))


(defn- next-real-line [state]
  (if (empty? (:current-tokens state))
    ;; No more tokens for current line, parse next line
    (loop [state state]
      (let [next-state (next-line state)
            tokens (line->tokens (:current-line next-state))
            next-state (assoc next-state :current-tokens tokens)
            more-input? (and (not (eof? next-state))
                             (empty? tokens))]

        (if more-input?
          (recur next-state)
          next-state))) 

    ;; Still tokens to process for the current line
    state))

(defn current-token [state]
  (-> state :current))

(defn next-token [state]
  (-> state :next))

(defn consume 
  ([state] (consume state nil))
  ([state _expected]
   
   (let [current (current-token state)
         state (next-real-line state)
         state (assoc state :current (:next state))
         state (assoc state :next (if (eof? state) 
                                    {:type :eof}
                                    (first (:current-tokens state))))
         state (assoc state :current-tokens (rest (:current-tokens state)))]
     
     ; Happens during first call to `consume`
     (if (nil? (:current state))
         (consume state)
         [current state]))))

(defn drop-token [lexer] 
  (let [[_ lexer] (consume lexer)] lexer))

(defn drop-if [lexer required-token]
  (if (= required-token (-> lexer current-token :type))
    (drop-token lexer)
    lexer))

(defn drop-required [lexer required-token]
  (if (= required-token (-> lexer current-token :type))
    (drop-token lexer)
    (assert false))) ;;Required token not found

(defn consume-if [lexer required-token]
  (if (= required-token (current-token lexer))
    (-> lexer drop-token consume)
    [nil lexer]))

(defn create [stream]
  (merge
   compiler-state
   {:source stream
    :source-lines (s/split-lines stream)}))  


(comment
  (print (read-string "\"hello \\\"wo\\nrld\" and some more")))