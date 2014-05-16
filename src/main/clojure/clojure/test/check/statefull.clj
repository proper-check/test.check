(ns clojure.test.check.statefull
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]))

(defn- make-fn [nm args & bd]
  [(keyword nm) `(fn ~args ~@bd)])

(defn- add-defaults [cmd]
  (merge {:next-state (fn [state] state)
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

(defn -add [l]
  (.add l 1))

(defn -get [l]
  (.get l 0))

(defn size [l]
  (.size l))

(defcommands circ-buff
  (init-m [] [])
  (init-r [] (ArrayList. 4))
  (clean [init-r])
  (command -add []
    (pre [state]
      (< (count state) 4))
    (next-state [state]
      (conj state 1)))

  (command -get []
    (pre [state]
      (seq state))
    (post [state res call]
      (= res (first state)))
    (next-state [state]
      (identity state)))

  (command size []
    (post [state res call]
          (= (count state) res))))

(def cmds [{:next-state inc
            :target inc
            :pre #(> % 0)}
           {:next-state identity
            :target identity
            :pre even?}])

(defn gen-command [cmds]
  (gen/one-of (mapv gen/return cmds)))

(defn gen-commands [cmds]
  (letfn [(gen-sized [state sz acc]
            (if (zero? sz)
              (gen/return acc)
              (gen/bind (gen/such-that #((:pre %) state)
                                       (gen-command cmds))
                        #(gen-sized ((:next-state %) state)
                                    (dec sz)
                                    (conj acc %)))))]
    (gen/sized #(gen-sized  0 % []))))

(gen/sample (gen-commands cmds))

(defn run-commands
  [{:keys [init-m init-r clean commands]}]
  (prop/for-all
   [rand-cmds (gen-commands commands)]
   (loop [rand-cmds rand-cmds
          state #_FIXINITSATE!!! 0
          real #_FIXINITSTATE!!! 0]
     (if (empty? rand-cmds)
       true
       (let [cmd (first rand-cmds)
             target (:target cmd)
             post (:post cmd)
             next-state (:next-state cmd)
             real-result (target real)]
         (if (post state real-result [])
           (recur (rest rand-cmds)
                  (next-state state)
                  real)
           false))))))

(tc/quick-check 100 (run-commands {:commands cmds}))


