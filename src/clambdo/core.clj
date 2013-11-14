(ns clambdo.core
  (:gen-class :main true))

(declare inject)
(declare leval)
(declare lapply)

(use '[clojure.core.match :only (match)])

(defn leval [e env]
  (match [e]
    [s :guard symbol?] (env s)
    [(['λ v b] :seq)]  `(closure ~e ~env)
    [([f v] :seq)]     #(lapply (leval f env) (leval v env))
    :else              (println "Error in leval!")))

(defn lapply [f e]
  (match [f]
    [([closure (['λ v b] :seq) env] :seq)] #(leval b (conj env {v e}))
    :else     (let []
                (println "Error in lapply!")
                (print "f: ") (println f) (newline) (print "e: ") (println e))))

(defn inject [code]
  (trampoline leval code {}))

(def test-exp
  '((λ x x) (λ x x)))

(def omega
  '((λ x (x x)) (λ x (x x))))

(defn -main []
  (println (inject test-exp)))

