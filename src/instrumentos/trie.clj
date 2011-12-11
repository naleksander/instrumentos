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
	(cons :node (interpose :node key)))

(defn tassoc[ col key item ]
	(assoc-in col (snoc (key-seq key) :val) item))

(defn tderef[ col ]
	(let [ rst (mapcat (fn[ [ k v] ] (tderef v) ) (:node col)) ]
		(if-let[ val (:val col) ]
			(snoc rst val)
			rst)))

(defn tderef-keys[ col word ]
	(let [ rst  (mapcat (fn[ [ k v] ] (tderef-keys v (str word k)) ) (:node col)) ]
		(if-let[ val (:val col) ]
			(cons word rst)
					rst)))
	
(defn tget[ col key ]
	(tderef (get-in col (key-seq key))))

(defn tkeys[ col key ]
	(tderef-keys (get-in col (key-seq key)) key))


(def g { :node { "a" { :val 2
  		               :node { "b" { :val 3
				  		             :node { "z" { :val 4
					     				           :node { "g" { :val 5 }}}}}}}
		         "b" { :val 22
		  		       :node { "b" { :val 33
						  		     :node { "z" { :val 44
							     				   :node { "g" { :val 55 }}}}}}}
		}})

(def g (-> {}
	(tassoc "a" 2)
	(tassoc "ab" 3)
	(tassoc "abz" 4)
	(tassoc "abzg" 5)
	(tassoc "brokowski" 4)
	(tassoc "b" 22)
	(tassoc "bb" 33)
	(tassoc "bbz" 44)
	(tassoc "bbzg" 55)
	(tassoc "brozowski" 5)))

(println (tkeys g "b"))


(println (tkeys (-> {}
	(tassoc "" 3)
	(tassoc "ala" 2)
	(tassoc "alaza" 4)
	(tassoc "a" 3)) "a"  ))

;(defn scaffold [iface] 
;  (doseq [[iface methods] (->> iface .getMethods 
;                            (map #(vector (.getName (.getDeclaringClass %)) 
;                                    (symbol (.getName %)) 
;                                    (count (.getParameterTypes %)))) 
;                            (group-by first))] 
;    (println (str "  " iface)) 
;    (doseq [[_ name argcount] methods] 
;      (println 
;        (str "    " 
;          (list name (into ['this] (take argcount (repeatedly 
;gensym))))))))) 



