(ns highlight-defined.core)

(defn- vars-in-ns [ns]
  (map second (ns-interns (ns-name ns))))

(defn- defined-vars []
  (->> (all-ns)
       (mapcat vars-in-ns)
       (into #{})))

(defmacro ^:private is-defined? [v]
  `(boolean ((defined-vars) (var ~v))))


