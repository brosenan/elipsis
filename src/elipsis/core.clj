(ns elipsis.core)

(defn ^:private transpose-list-of-maps [list-of-maps]
  (reduce (fn [m [k v]]
            (assoc m k (conj (get m k []) v)))
          {} (apply concat list-of-maps)))


(declare deconstruct)

(defn deconstruct-elipsis [pattern terms]
  (cond
    (symbol? pattern) (deconstruct pattern terms)
    :else (let [envs (map (partial deconstruct pattern) terms)]
            (transpose-list-of-maps envs))))

(defn deconstruct-seq [pattern term]
  (if (empty? pattern)
    (if (empty? term)
      {}
      ; else
      nil)
    ; else
    (if (empty? term)
      nil
      ; else
      (if (= (second pattern) '...)
        (deconstruct-elipsis (first pattern) term)
        ; else
        (let [rest (deconstruct-seq (rest pattern) (rest term))]
          (if (nil? rest)
            nil
            ; else
            (let [fst (deconstruct (first pattern) (first term))]
              (if (nil? fst)
                nil
                ; else
                (merge fst rest)))))))))

(defn deconstruct [pattern term]
  (cond
    (symbol? pattern) {pattern term}
    (not= (class pattern) (class term)) nil
    (sequential? pattern) (deconstruct-seq pattern term)
    :else (if (= pattern term)
            {}
            ; else
            nil)))

(declare reconstruct)

(defn common-length [pattern env]
  (cond
    (symbol? pattern) (count (env pattern))
    (sequential? pattern) (let [len (common-length (first pattern) env)]
                            (if (empty? (rest pattern))
                              len
                              ; else
                              (if (not= (common-length (rest pattern) env) len)
                                (throw (Exception. (str "Trying to reconstruct patterns of different lengths: " pattern)))
                                ; else
                                len)))
    :else 3))

(defn slice-at-index [env index]
  (into {} (map (fn [[k v]] [k (if (sequential? v)
                                 (nth v index)
                                 ; else
                                 nil)]) env)))

(defn reconstruct-elipsis [pattern env]
  (if (symbol? pattern)
    (reconstruct pattern env)
    ; else
    (let [len (common-length pattern env)
          envs (map (partial slice-at-index env) (range len))]
      (map (partial reconstruct pattern) envs))))

(defn reconstruct-seq [pattern env]
  (if (empty? pattern)
    pattern
    ; else
    (if (= (second pattern) '...)
      (reconstruct-elipsis (first pattern) env)
      ; else
      (cons (reconstruct (first pattern) env) (reconstruct-seq (rest pattern) env)))))

(defn reconstruct [pattern env]
  (cond
    (and (symbol? pattern)
         (env pattern)) (env pattern)
    (vector? pattern) (vec (reconstruct-seq (seq pattern) env))
    (seq? pattern) (reconstruct-seq pattern env)
    :else pattern))
