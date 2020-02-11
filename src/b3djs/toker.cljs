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

(defn- next-line [state]
  ;;Inc row
  (-> state
      (assoc :current-line (first (:source-lines state)))
      (assoc :source-lines (rest (:source-lines state)))))

(defn- eof? [state] (empty? (:source-lines state)))

(defn read-ident [line] 
  (let [[ident remain] (split-with valid-ident-char? line)]
    [{:type :ident :value (apply str ident)} remain]))

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
(defn read-number [str] 
  [{:type :number} (drop-while numeric? str)])

(defn parse-token [str]
  (let [[c n1 n2] (take 3 str)]
    (cond
      (nil? c) nil
      
      (s/blank? c) [nil (drop 1 str)]
      (= c ";") [nil ""]

      (and (= c ".") (numeric? n1)) [{:type :num} (rest str)];;(read-number str)
      (and (= c "-") (= n1 ".") (numeric? n2)) [{:type :num} (rest str)];;(read-number str)
      (or (numeric? c)
          (and (= c '-') (numeric? n1))) [{:type :num} (rest str)];;(read-number str)
      
      (= c "$") (read-hex str)
      (= c "%") (read-binary str)
      (= c "\"") (read-string str)
      (alpha? c) (read-ident str)

      (and (= c "<") (= n1 "=")) [{:type :lte} (drop 2 str)]
      (and (= c ">") (= n1 "=")) [{:type :gte} (drop 2 str)]
      (and (= c "<") (= n1 ">")) [{:type :ne} (drop 2 str)]

      (simple-tokens c) [{:type (simple-tokens c)} (drop 1 str)]
      :else [{:type :unknown} (drop 1 str)])))

(defn line->tokens [str]
  (loop [remain str
         tokens []]
    
    (if-let [[token res] (parse-token remain)]
      (recur res (if token (conj tokens token) tokens))
      tokens)))


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

(defn consume 
  ([state] (consume state nil))
  ([state _expected]
   
   (let [state (next-real-line state)
         state (assoc state :current (:next state))
         state (assoc state :next (if (eof? state) 
                                    {:type :eof}
                                    (first (:current-tokens state))))
         state (assoc state :current-tokens (rest (:current-tokens state)))]
     
     ; Happens during first call to `consume`
     (if (nil? (:current state))
         (consume state)
         state))))

(defn create [stream]
  (merge
   compiler-state
   {:source stream
    :source-lines (s/split-lines stream)}))  


(comment
  (print (read-string "\"hello \\\"wo\\nrld\" and some more")))