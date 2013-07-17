(ns snake.macros)

(defn debug-fn [form]
  (if (list? form)
    `(let [result# (~(first form) ~@(map #(debug-fn %) (rest form)))]
       (console/log (str '~form " => " result#))
       result#)
    form))

(defmacro debug [form]
  (debug-fn form))
