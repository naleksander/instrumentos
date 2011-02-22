(ns instrumentos.core
	(:use (instrumentos yield)))



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
		(Thread/sleep 2000)))

