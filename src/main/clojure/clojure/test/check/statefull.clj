(ns clojure.test.check.statefull
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

(defn- make-fn [nm args & bd]
  [(keyword nm) `(fn ~args ~@bd)])

(defn- add-defaults [cmd]
  (merge {:next-state (fn [state res call] state)
          :post (constantly true)
          :pre (constantly true)}
         cmd))

(defn- describe [nm args & body]
  (let [pairs [[:target nm]
               [:args args]]]
    (->> body
         (reduce (fn [acc fun]
                    (conj acc (apply make-fn fun)))
                 pairs)
         (into {})
         (add-defaults))))

(defn- extract-commands [body]
  (map (fn [d]
         (apply describe (rest d)))
       body))

(defmacro defcommands [nm & body]
  `(def ~nm ~(vec (extract-commands body))))

#_(defcommands circ-buff
  (init-m [])
  (init-r [])
  (clean [init-r])
  (describe -put [gen/int]
    (pre [state call]
      (< (count state) buf-size))
    (next-state [state res [fun arg]]
      (concat state [arg])))

  (describe -get []
    (pre [state call]
      (seq state))
    (post [state res call]
      (= res (first state)))
    (next-state [state res call]
      (rest state)))

  (describe -size []
    (post [state res call]
          (= (count state) res))))

(defn generate-calls [cmds]
  (gen/not-empty
    (gen/vector
      (gen/one-of
        (map (fn [{:keys [args target]}]
               (if (empty? args)
                 (gen/tuple (gen/return target))
                 (apply gen/tuple (gen/return target) args)))
             cmds)))))

(defn find-command [cmds cmd]
  (some (fn [{target :target :as full-cmd}]
          (when (= target cmd)
            full-cmd))
        cmds))

(defn gen-props [commands]
  (prop/for-all
   [fns (generate-calls commands)]
   (loop [fns fns
          model (init-model)
          real (init-real)]
     (if (empty? fns)
       (do (cleanup!) true)
       (let [[fun & args] (first fns)
             {:keys [next-state post pre]}
             (find-command commands fun)]
         (if-not (pre model args)
           ;; skip this seq of commands
           :pre
           (let [real-result (apply fun real args)]
             (if (post model real-result (cons real args))
               (recur (rest fns) 
                      (next-state model real-result 
                                  (cons real args)) real)
               (do (cleanup!) false)))))))))
