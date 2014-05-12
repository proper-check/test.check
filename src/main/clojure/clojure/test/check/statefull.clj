(ns clojure.test.check.statefull
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]))

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
  [:commands (mapv (fn [d]
                    (apply describe (rest d)))
                  body)])

(defmacro defcommands [nm init-m init-r clean & cmds]
  `(def ~nm ~(into {} (vector (apply make-fn init-m)
                              (apply make-fn init-r)
                              (apply make-fn clean)
                              (extract-commands cmds)))))

(import 'java.util.ArrayList)

(defn -add [l i]
  (.add l i))

(defn -get [l]
  (.get l 0))

(defn size [l]
  (.size l))

(defcommands circ-buff
  (init-m [] [])
  (init-r [] (ArrayList. 4))
  (clean [init-r])
  (command -add [gen/int]
    (pre [state call]
      (< (count state) 4))
    (next-state [state res [fun arg]]
      (conj state arg)))

  (command -get []
    (pre [state call]
      (seq state))
    (post [state res call]
      (= res (first state)))
    (next-state [state res call]
      (identity state)))

  (command size []
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

(gen/sample (generate-calls (:commands circ-buff)))

(defn find-command [cmds cmd]
  (some (fn [{target :target :as full-cmd}]
          (when (= target cmd)
            full-cmd))
        cmds))

(defn gen-command [commands]
  (gen/one-of (mapv gen/return commands)))

(defn gen-commands [commands]
  (letfn [(sized-commands [state size]
            (println state)
            (if (zero? size)
              nil
              (let [command (gen/such-that
                             #((:pre %) state)
                             (gen-command commands))]
                (gen/return
                 (cons command
                       (gen/bind command
                                 (fn [cmd]
                                   (sized-commands ((:next-state cmd) state) (dec size)))))))))]
    #_(gen/sized #(sized-commands 0 %))
    (sized-commands 0 1)))

(defn test [size]
  (if (zero? size)
    nil
    (cons size (test (dec size)))))

(gen/sample (gen/gen-bind
             (gen/sequence gen/gen-bind gen/gen-pure [gen/int gen/int])
             (fn [roses]
               (gen/gen-pure (gen/zip-rose clojure.core/vector roses)))) 2)

(def cmds [{:next-state inc
            :pre (constantly true)}
           {:next-state identity
            :pre (constantly false)}])

(gen/sample (gen/such-that #((:pre %) 1) (gen-command cmds)) 1)
(gen/sample (gen-commands cmds))

(gen-commands cmds)

(gen/sample (gen/such-that odd? gen/int))


(defn gen-props
  [{:keys [init-m init-r clean commands]}]
  (prop/for-all
   [fns (generate-calls commands)]
   (loop [fns fns
          model (init-m)
          real (init-r)]
     (if (empty? fns)
       (do (clean real) true)
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
               (do (clean real)
                   (println "failed")
                   (println "(" fun real args ") => " real-result)
                   false)))))))))

(tc/quick-check 100 (gen-props circ-buff))
