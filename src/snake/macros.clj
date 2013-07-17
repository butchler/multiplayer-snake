(ns snake.macros)

(defmacro debug [form]
  (if (list? form)
    `(let [result# (~(first form) ~@(map #(debug %) (rest form)))]
       (console/log (str '~form " => " result#))
       result#)
    form))
