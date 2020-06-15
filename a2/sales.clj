;Tianlin Yang ID:40010303
;2020/06/06

;Run program by command line: cd /location   ,than:   clj sales.clj
;Please make sure sales/cust/prod.txt in same folder / cd.
;Also tested running from IDEA Cursive

; REFERENCES
; https://stackoverflow.com/questions/9047231/read-a-file-into-a-list-each-element-represents-one-line-of-the-file
; https://stackoverflow.com/questions/52446937/clojure-multiple-tasks-in-if


(ns sales     ;Name space for file, if wrong on your computer please change to right position.
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  )

;;----------------------------------------- Functions library --------------------------------------

;;----------------- Read file to vector
(defn read-to-vec [file]
  (with-open [rdr (io/reader file)]
    (reduce conj [] (line-seq rdr))));read line by line, save each line as single vector


;;----------------- Convert vector to v-v list
(defn vec-to-vec [v]
  (map #(seq (.split % "\\|")) v))


;;----------------- Now get the map
(defn vec-to-map [v]
  (into {} (for [x v] (hash-map (Integer. (first x)) x))))
(defn vec-to-NameMap [v]
  (into {} (for [x v] (hash-map (nth x 1) x))))


;;----------------- Now sorting the map
(def CustMap (vec-to-vec (read-to-vec "cust.txt")))
(def sortMap (into(sorted-map) (vec-to-map CustMap)))
(def sortName (into(sorted-map) (vec-to-NameMap CustMap)))

(def prodMap (vec-to-vec (read-to-vec "prod.txt")))
(def sortProd (into(sorted-map) (vec-to-map prodMap)))
(def sortProdName (into(sorted-map) (vec-to-NameMap prodMap)))

(def sortSale (into(sorted-map) (vec-to-map (vec-to-vec (read-to-vec "sales.txt")))))

;;----------------- Calculator for total money
(defn getLinePrice [id,num]
(* (Double/parseDouble num) (Double/parseDouble (nth (get sortProd (Integer. id)) 2)))
  )

(defn extraBuyHist [line]
  (def p {:price 0})
  (def id (nth line 0))
  (doall
    (for [[k v] sortSale]
      (if (= (nth v 1) id)
        (def p(update p :price + (getLinePrice (nth v 2) (nth v 3))));update to price variable map
        ();do nothing when not founded
        )
      )
    )
  (println "Total sales:$"(get p :price));now print out total value.
  )

;;----------------- Calculator for total count of product sale
(defn countProduct [id]
  (def c {:count 0})
  (doall
    (for [[k v] sortSale]
      (if (= id (nth v 2))
        (def c (update c :count + (Integer. (nth v 3)))) ;update to price variable map
        () ;do nothing
        )
      )
    )
  (if (> (get c :count) 0)
    (println "Total count:"(get c :count))
    (println "*** Product name not founded!")
    )
  )

;;----------------- Printer for Customer format output
(defn printCust [m]
  (print "-------------------------------------------------\n")
  (print "ID  Name        Address             Number\n")
  (print "-------------------------------------------------\n")
  (doall (for [[k v] m] (println (nth v 0)","(nth v 1)","(nth v 2)","(nth v 3))
                        ))
  (print "-------------------------------------------------\n"))

;;----------------- Printer for Product format output
(defn printProd [m]
  (print "-------------------------------------------------\n")
  (print "ID  Product     Cost              \n")
  (print "-------------------------------------------------\n")
  (doall (for [[k v] m] (println (nth v 0)","(nth v 1)","(nth v 2))
                        ))
  (print "-------------------------------------------------\n"))

;;----------------- Printer for Sales format output
(defn IdToName [id]
  (nth (get sortMap (Integer. id)) 1))
(defn IdToGoods [id]
  (nth (get sortProd (Integer. id)) 1))

(defn printSale [m]
  (print "-------------------------------------------------\n")
  (print "salesID   CustName      ProdName     ItemCount     \n")
  (print "-------------------------------------------------\n")
  (doall (for [[k v] m] (println (nth v 0)",      "(IdToName (nth v 1))",    "(IdToGoods (nth v 2))",       "(nth v 3))
                        ))
  (print "-------------------------------------------------\n"))



;;--------------------------------------- Function library end here -------------------------------------




;;--------------------------------------Main UI program----------------------------------------
;waiting for user to make an choose

(defn main-loop []
  ; print option screen
  (println "\n*** Sales Menu *** \n------------------\n1. Display Customer Table\n2. Display Product Table\n3. Display Sales Table\n4. Total Sales for Customer\n5. Total Count for Product\n6. Exit\n\nEnter an option?")
  (case (read-line)
    "1" (do (println )(printCust sortMap) (recur))
    "2" (do (println )(printProd sortProd) (recur))
    "3" (do (println )(printSale sortSale) (recur))
    "4" (do (println "Please enter the name(Case-sensitive):")
            (let [inputName (read-line)]
              (def l (get sortName inputName))
              (if (contains? sortName inputName)
                (extraBuyHist l)
                (println "*** Name not founded!")
                )
              )
            (recur))
    "5" (do (println "Please enter the product name(Case-sensitive):")
            (let [inputName (read-line)]
              (def id (nth (get sortProdName inputName) 0));translate "name" to id(String)
              (countProduct id)
              )
            (recur))
    "6" (do (println )(println "Exit, Thank you for your time! By Tianlin Yang"))
    (do (println "got invalid command!") (recur))))

(main-loop)

(println "Grading finished! Have an nice day!")

;;----------------------------------------------------------------------------------------------