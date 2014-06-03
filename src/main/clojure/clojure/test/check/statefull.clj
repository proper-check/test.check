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

(def cmds {:clean identity
           :cmds {:-new {:fn (fn [r] (ArrayList.))
                         :fn-name :-new
                         :next-state (fn [s] [])
                         :pre (fn [s] true)
                         :post (fn [s r args] (not (nil? r)))}
                  :add  {:fn (fn [r] (-add r))
                         :fn-name :add
                         :next-state (fn [s] (conj s 1))
                         :pre (fn [s] (not (nil? s)))
                         :post (fn [s r args] true)}
                  :get  {:fn (fn [r] (-get r))
                         :fn-name :get
                         :next-state (fn [s] s)
                         :pre (fn [s] (seq s))
                         :post (fn [s r args] (= (last s) r))}}})

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
    (gen/sized #(gen-sized nil % []))))

(gen/sample (gen-commands (vals (:cmds cmds))))

(defn run-commands
  [{:keys [cmds]}]
  (prop/for-all
   [rand-cmds (gen-commands (vals cmds))]
   (loop [rand-cmds rand-cmds
          state nil
          real nil]
     (if (empty? rand-cmds)
       true
       (let [cmd (first rand-cmds)
             target (:fn cmd)
             post (:post cmd)
             next-state (:next-state cmd)
             real-result (target real)]
         (if (post state real-result [])
           (recur (rest rand-cmds)
                  (next-state state)
                  (or real real-result))
           false))))))

(tc/quick-check 100 (run-commands cmds))
