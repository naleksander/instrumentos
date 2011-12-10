(ns instrumentos.trie)

(defn snoc[ col item ]
	(lazy-seq
		(if (seq col)
			(let[ [f &  r] col ]
				(if (seq r)
					(cons f (snoc r item))
					(cons f [item])))
			[item])))

(defn key-seq[ key ]
	(mapcat #(vector %1 (str %2)) (repeat :node) (seq key)))

(defn tassoc[ col key item ]
	(assoc-in col (snoc (key-seq key) :val) item))

(defn tderef[ col ]
	(let [ rst (mapcat (fn[ [ k v] ] (tderef v) ) (:node col)) ]
		(if-let[ val (:val col) ]
			(snoc rst val)
			rst)))

(defn tget[ col key ]
	(tderef (get-in col (key-seq key))))

;
;(tderef (tget g ""))
;
;(tderef g)
;
;(tderef (tget g "ab"))

(def g { :node { "a" { :val 2
  		               :node { "b" { :val 3
				  		             :node { "z" { :val 4
					     				           :node { "g" { :val 5 }}}}}}}
		         "b" { :val 22
		  		       :node { "b" { :val 33
						  		     :node { "z" { :val 44
							     				   :node { "g" { :val 55 }}}}}}}
		}})


(def z (-> {}
	(tassoc "" 3)
	(tassoc "ala" 2)
	(tassoc "alaza" 4)
	(tassoc "a" 3)))

(tassoc {} "" 3)

(println z)

(println (tget z "alaz"))

(snoc (snoc (snoc [ 1 2 3] 4) 6) 7)



