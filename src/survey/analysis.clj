; Parse csv and produce some interesting correlations and statistics

(ns survey.analysis
  (:require [clojure.java.io :as io]
            [clojure.string :as STR :only (join split)])
  )


; Define some constants, just to help out with this version of the .csv

(def name-row 0)
(def gender-row 1)
(def age-row 2)
(def first-question-row 3)
(def last-question-row 17)




; Convert a csv from data format to a sequence of lines
(defn csv-to-array [file-path]
  (let [rdr (io/reader file-path)
        lines (line-seq rdr)]
    (loop
      [flines lines
       line-array []]

      (if (empty? flines) line-array
        (recur (rest flines) (conj line-array (first flines)))
        )
      )
    )
  )


; Extract column data
(defn get-column-data [line column]
  ((STR/split line #"\,") column)
  )

; remove comma from a list and return a list
(defn de-comma [comma-separated]
  (STR/split comma-separated #"\,")
  )

; put age into a bucket
(defn age-bucket [age]
  (cond
   (<= age 30) 1
   (and (> age 30) (<= age 60)) 2
   (and (> age 60) (<= age 100)) 3
   )
  )

; convert quoted string into an integer
(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

; predicate to determine if male
(defn is-male? [s]
  (cond
   (= "m" s) true
   (= "M" s) true
   ) )


; predicate to determin if female
(defn is-female? [s]
  (cond
   (= "f" s) true
   (= "F" s) true
   ) )


; is a yes answer
(defn is-yes? [answer]
  (if (= answer 1) true false))

; is a no answer
(defn is-no? [answer]
  (if (= answer 0) true false))


(defn question-text [question data]
  (first (de-comma (data (+ question 2))))
  )


; given a question and the data, categorize the answers in yes/no format
(defn answers-to-question [question data]
  (let [yes (filter is-yes? (map parse-int (rest (de-comma (data (+ question  2))))))
        no  (filter is-no? (map parse-int (rest (de-comma (data  (+ question  2))))))]
    {:yes (count yes) :no (count no)})
  )


(defn do-histogram [question data]
  (let [yes-no (answers-to-question question data)]
    (view (pie-chart ["no" "yes"] [(yes-no :no) (yes-no :yes)] :title (question-text question data)))))

(defn do-sentences [question data]
  (let [yes-no (answers-to-question question data)
        total (+ (yes-no :yes) (yes-no :no))
        yp (int (/ (* 100 (yes-no :yes)) total))
        np (- 100 yp)]

    (spit "answers.txt" (str "Yes:" yp "%" "\tNo:" np "%\t"(question-text question data)  "\n") :append true)
    )
  )

(defn answers-match? [a0 a1]
  (if (= a0 a1) 1 0))

(defn answers [person data]
  (loop
    [line first-question-row
     accum ()]
    (if (> line last-question-row) (reverse (map parse-int (rest accum)))
      (recur (inc line) (conj  accum (get-column-data (data line) person ))))
    )
  )

(defn correlation-count [p0 p1 data]
  (reduce + (map answers-match? (answers p0 data) (answers p1 data)))
  )

(defn correlation-data-pairs [p0 p1 data]
  {:p0 (person-id p0 data) :p1 (person-id p1 data) :same (correlation-count p0 p1 data)})



(defn person-id [p data]
  (str (get-column-data (data 0) p) ":" (get-column-data (data 1) p) ":" (get-column-data (data 2) p))
  )


(defn compare-data-pairs [p0 p1]
  (compare (p0 :same) (p1 :same)))

(defn generate-data-pairs [p data max]
  (loop [candidate 1
         accum []]

    (if (> candidate max) accum
      (recur (inc candidate) (if (= candidate p ) accum (conj accum (correlation-data-pairs p candidate data)))))
    ))


(defn correlation-count-using-data [p0 p1]
  (int(sum (map answers-match? (answers (quot p0 1) data) (answers (quot p1  1) data)))
  ))


(defn top [p data]
  (let [sorted-list (sort compare-data-pairs (generate-data-pairs p data num-people))
        best-score (last sorted-list)
        best (best-score :same)]

  (filter (fn [m] (= (m :same) best)) sorted-list))


  )

;
; start of analysys
;

; slurp in the csv to a big array
(def data (csv-to-array "/Users/taylor/Documents/leinprojects/survey/resources/2034.csv"))

; go grab the ages
(def ages (de-comma (data age-row)))

; determine the number of people in the survey by the size of thage ages
(def num-people (- (count ages) 1))

; turn the ages from string to integers
(def ages-int (map parse-int (rest ages)))

; go grab all the genders and filter on males/females
(def gender (rest(de-comma (data gender-row))))
(def males (filter is-male? gender))
(def females (filter is-female? gender))

(use '(incanter core charts stats))
(mean ages-int)
(median ages-int)

;(view (histogram ages-int :nbins 10 :x-label "age" :title "age distribution") )




;(view (pie-chart ["male" "female"] [(count males) (count females)]))   

;(do-histogram 12 data)
;(for [q (range 1 15
;)] (do-histogram q data))




(for [q (range 1 40)] (println q "->"  (last (top q data)) :same))






;(view (heat-map correlation-count-using-data 1 40 1 40))

;(do-histogram 12 data)
;(io/delete-file "answers.txt")
;(for [q (range 1 15)] (do-sentences q data))

