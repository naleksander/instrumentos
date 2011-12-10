(ns instrumentos.core
	(:use (instrumentos yield)))

;(load-file "instrumentos/yield.clj")
;(create-ns  'instrumentos.yield)
;(remove-ns 'instrumentos.core)
;(remove-ns 'instrumentos.yield)
;(abc 2)

(comment
(let[ o (Object.) ]
	(defn sprintln[ & l ]
		(locking o
			(apply println l)
				)))


(def z (yieldish 
	(doseq[ x (range 10) ]
		(sprintln "producing" x)
		  	(yield x))))


(do
(Thread/sleep 2000)
(doseq[ x z ]
	(sprintln "consuming" x)
		(Thread/sleep 2000))))

(doseq[ x (yieldish 
  (doseq [i (range 3)] 
    (yield i)) 
  (if true 
    (yield "hello world") 
    (yield "dlrow olleh")) 
  (doseq [j (range 2)] 
    (doseq [k (range 2)] 
      (yield [j k])))) ]
	(println x))

