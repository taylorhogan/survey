; Parse csv and produce some interesting correlations and statistics

(ns survey.analysis
  (:require [clojure.java.io :as io]
            [clojure.string :as STR :only (join split)]
            [clojure.set :as SET]
           )
  )

(use '(incanter core charts stats))

; Define some constants, just to help out with this version of the .csv

(def name-row 0)
(def gender-row 1)
(def age-row 2)
(def first-question-row 3)
(def last-question-row 17)



;
(defn all-pairs [col]
  (loop [[x & xs] col
         result []]

    (if (nil? xs)
      result
      (recur xs (concat result (map #(vector x %) xs))))))


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

; remove columns of data based on a filtering method provided
(defn filtered-data [d f]

  )




; Extract column data
(defn get-column-data [line column]
  ((STR/split line #"\,") column)
  )

; remove comma from a list and return a list
(defn de-comma [comma-separated]
  (STR/split comma-separated #"\,")
  )

; get resource path
(defn resource-path []
  (let [current-directory (System/getProperty "user.dir")]
    (if (.contains current-directory "/src") (str current-directory "/../../resources") (str current-directory "/resources"))
    ))


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
    ))


; predicate to determin if female
(defn is-female? [s]
  (cond
    (= "f" s) true
    (= "F" s) true
    ))


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
  (let [yes (filter is-yes? (map parse-int (rest (de-comma (data (+ question 2))))))
        no (filter is-no? (map parse-int (rest (de-comma (data (+ question 2))))))]
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

    (spit "answers.txt" (str "Yes:" yp "%" "\tNo:" np "%\t" (question-text question data) "\n") :append true)
    )
  )

(defn answers-match? [a0 a1]
  (if (= a0 a1) 1 0))

(defn answers [person data]
  (loop
      [line first-question-row
       accum ()
       ]
    (if (> line last-question-row) (reverse (map parse-int (rest accum)))
                                   (recur (inc line) (conj accum (get-column-data (data line) person))))
    )
  )

(defn correlation-count [p0 p1 data]
  (reduce + (map answers-match? (answers p0 data) (answers p1 data)))
  )


(defn person-id [p data]
  (str (get-column-data (data 0) p) ":" (get-column-data (data 1) p) ":" (get-column-data (data 2) p))
  )

(defn correlation-data-pairs [p0 p1 data]
  {:p0 (person-id p0 data) :p1 (person-id p1 data) :same (correlation-count p0 p1 data)})



(defn compare-data-pairs [p0 p1]
  (compare (p0 :same) (p1 :same)))


(defn generate-data-pairs [p data max]
  (loop [candidate 1
         accum []]

    (if (> candidate max) accum
                          (recur (inc candidate) (if (= candidate p) accum (conj accum (correlation-data-pairs p candidate data)))))
    ))



(defn top [p data num-people]
  (let [sorted-list (sort compare-data-pairs (generate-data-pairs p data num-people))
        best-score (last sorted-list)
        best (best-score :same)]

    (filter (fn [m] (= (m :same) best)) sorted-list))

  )

(defn worst [p data num-people]
  (let [sorted-list (sort compare-data-pairs (generate-data-pairs p data num-people))
        worst-score (first sorted-list)
        the-worst (worst-score :same)]

    (filter (fn [m] (= (m :same) the-worst)) sorted-list))

  )
(defn number-at-match-count [col count]

  (filter #(= count %) col)
  )




(defn find-matches [pair-list d]
  (map (fn [pl] (correlation-count (first pl) (last pl) d)) pair-list)
  )

;
; start of analysys
;

; slurp in the csv to a big array


(def file-path (str (resource-path) "/2034.csv"))
(def csv-data (csv-to-array file-path))

; go grab the ages
(def ages (de-comma (csv-data age-row)))

; determine the number of people in the survey by the size of thage ages
(def num-people (- (count ages) 1))


; turn the ages from string to integers
(def ages-int (map parse-int (rest ages)))

; go grab all the genders and filter on males/females
(def gender (rest (de-comma (csv-data gender-row))))
(def males (filter is-male? gender))
(def females (filter is-female? gender))


(mean ages-int)
(median ages-int)

;(view (histogram ages-int :nbins 10 :x-label "age" :title "age distribution") )

;(view (pie-chart ["male" "female"] [(count males) (count females)]))   

;(for [q (range 1 15)] (do-histogram q data))

;(for [q (range 1 40)] (println q "->"  (last (top q data num-people)) :same))






;(view (heat-map correlation-count-using-data 1 40 1 40))

;(do-histogram 12 data)
;(io/delete-file "answers.txt")
;(for [q (range 1 15)] (do-sentences q data))

; determine the corrleation between people based on matching answers

;(def apairs (all-pairs (range 1 num-people)))
;(def matches (find-matches apairs csv-data))
;(view (histogram matches :x-label "number of matches" :title "Frequency of Answer Matches"))




(defn get-column [c d]
  (loop
    [lines d
     answer ()]

     (if (empty? lines) answer
       (recur (rest lines) (conj answer (get-column-data (first lines) c)))))
  )


(first csv-data)

(get-column-data (first csv-data) 3)

(csv-data 0)

(def m (find-matches '((2 1 ) (2 3) (2 4) (2 5) (2 6) (2 7) (2 8) (2 9) (2 10)
                      (2 11 ) (2 12)(2 13) (2 14) (2 15) (2 16) (2 17) (2 18) (2 19) (2 20)
                      (2 21 ) (2 22)(2 23) (2 24) (2 25) (2 26) (2 27) (2 28) (2 29) (2 30)
                      (2 31 ) (2 32)(2 33) (2 34) (2 35) (2 36) (2 37) (2 38) (2 39) (2 40)
                      (2 41 ) (2 42)(2 43) (2 44) (2 45) (2 46) (2 47) (2 48) (2 49) (2 50)) csv-data))

(view (bar-chart '(1 2 3 4 5 6 7 9 9 10
                     11 12 13 14 15 16 17 18 19 20
                     21 22 23 24 25 26 27 28 29 30
                     31 32 33 34 35 36 37 38 39 40
                     41 42 43 44 45 46 47 48 49 50) m :x-label "other person" :title "Answers in Common With Taylor"))


(worst 5 csv-data 50)

