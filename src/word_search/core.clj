(ns word-search.core
  (:require
   [clojure.java.io :as io])
  (:import
   (java.io BufferedReader)))

(def matrix
  (with-open [^BufferedReader rdr (io/reader "resources/input.txt")]
    (->> (line-seq rdr)
         (map #(into [] (seq %)))
         (into []))))

(defn path [from to]
  (let [[x1 y1] from
        [x2 y2] to
        xstep (if (< x1 x2) 2 -2)
        ystep (if (< y1 y2) 1 -1)]
    (cond
      (= y1 y2) (map (fn [x] [x y1]) (range x1 (+ x2 xstep) xstep))
      (= x1 x2) (map (fn [y] [x1 y]) (range y1 (+ y2 ystep) ystep))
      :else (map vector
                 (range x1 (+ x2 xstep) xstep)
                 (range y1 (+ y2 ystep) ystep)))))

(defn path->word [from to]
  (->> (path from to)
       (map (fn [[x y]]
              (nth (nth matrix (dec y)) x)))
       (apply str)))

(defn point-seq []
  (for [y (range 1 (inc (count matrix)))
        x (range (inc (count (nth matrix (dec y)))))]
    [x y]))

(defn point-pairs []
  (for [from (point-seq)
        to (point-seq)
        :let [[x1 y1] from
              [x2 y2] to]
        :when (and (even? x1)
                   (even? x2)
                   (or (= x2 x1)
                       (= y2 y1)
                       (= (/ (abs (- x2 x1)) 2)
                          (abs (- y2 y1)))))]
    [from to]))

(defn search [words]
  (apply merge
         (for [[from to] (point-pairs)
               :let [w (path->word from to)]
               :when (words w)]
           {[(reverse from) (reverse to)] w})))

(search #{"BEAKER" "ERLENMEYER" "FLASK" "TESTTUBES"
          "MEASURINGCYLINDER" "VOLUMETRICFLASK" "PIPETTE"
          "BRUSH" "PASTEURPIPETTE" "SPATULA" "MORTAR"
          "FUNNEL" "SLIDES" "COVERSLIP" "WATCHGLASS"
          "WASHBOTTLE" "DROPPERVIAL" "ELECTRONICSCALE"
          "THERMOMETER" "PIPETTEASPIRATOR" "SCALPEL"
          "STIRRINGROD" "PETRIDISH" "DISSECTIONEQUIPMENT"
          "BUNSENBURNER"})
