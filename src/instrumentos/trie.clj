(ns instrumentos.trie)

(defn conr[ col item ]
	(lazy-seq
		(if (seq col)
			(let[ [f &  r] col ]
				(if (seq r)
					(cons f (conr r item))
					(cons f [item])))
			[item])))

(defn tire-key-seq[ key ]
	(cons :node (interpose :node key)))

(defn tire-assoc[ col key item ]
	(assoc-in col (conr (tire-key-seq key) :val) item))

(defn deref-tire-vals[ col ]
	(let [ rst (mapcat (fn[ [ k v] ] (deref-tire-vals v) ) (:node col)) ]
		(if-let[ val (:val col) ]
			(conr rst val)
			rst)))

(defn deref-tire-keys[ col word ]
	(let [ rst  (mapcat (fn[ [ k v] ] (deref-tire-keys v (str word k)) ) (:node col)) ]
		(if-let[ val (:val col) ]
			(cons word rst)
					rst)))

(defn tire-vals
	([ col ]
		(deref-tire-vals col))
	([ col key ]
		(if (seq key)
			(deref-tire-vals (get-in col (tire-key-seq key)))
			(tire-vals col))))

(defn tire-keys
	([ col ]
		(deref-tire-keys col ""))
	([ col key ]
		(if (seq key)
			(deref-tire-keys (get-in col (tire-key-seq key)) key)
			(tire-keys col))))


;(def g (-> {}
;	(tire-assoc "a" 2)
;	(tire-assoc "ab" 3)
;	(tire-assoc "abz" 4)
;	(tire-assoc "abzg" 5)
;	(tire-assoc "brokowski" 4)
;	(tire-assoc "b" 22)
;	(tire-assoc "bb" 33)
;	(tire-assoc "bbz" 44)
;	(tire-assoc "bbzg" 55)
;	(tire-assoc "brozowski" 5)))
;
;(tire-keys g "a")


;(def g { :node { "a" { :val 2
;  		               :node { "b" { :val 3
;				  		             :node { "z" { :val 4
;					     				           :node { "g" { :val 5 }}}}}}}
;		         "b" { :val 22
;		  		       :node { "b" { :val 33
;						  		     :node { "z" { :val 44
;							     				   :node { "g" { :val 55 }}}}}}}
;		}})
;
;
;(println (tvals g ))
;
;(println (tkeys (-> {}
;	(tassoc "" 3)
;	(tassoc "ala" 2)
;	(tassoc "alaza" 4)
;	(tassoc "a" 3)) "a"  ))
;
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
;
;
; (deftype Trie [^clojure.lang.IPersistentMap trie]
;  Object
;    (toString [this]
;	  (str trie)) 
;    (hashCode [this] 
;      (.hashCode trie)) 
;    (equals [this y] 
;      (.equals trie y)) 
;  clojure.lang.IPersistentMap 
;    (without [this G__443]) 
;    (assocEx [this G__444 G__445]) 
;  java.lang.Iterable 
;    (iterator [this]
;		(.iterator [])) 
;  clojure.lang.Associative 
;    (containsKey [this G__446]) 
;    (assoc [this key item]
;		(Trie. 
;			(tassoc trie key item)))
;    (entryAt [this G__449]) 
;  clojure.lang.IPersistentCollection 
;    (cons [this G__450] this) 
;    (empty [this] false) 
;    (equiv [this x]
;		(.equiv trie x)) 
;  clojure.lang.Seqable 
;    (seq [this]
;		(tvals trie)) 
;  clojure.lang.ILookup 
;    (valAt [this G__452 G__453]) 
;    (valAt [this G__454]) 
;  clojure.lang.Counted 
;    (count [this]))
;
;(defn trie[]
;	(Trie. {}))
;
;(seq (assoc (trie) "ala" 3))
;
;(seq (assoc (trie) "ala" 3))
;
;;(seq (tget (assoc (trie) "ala" 3) "ala"))
;
;;(seq (assoc (trie) "ala" 2))
;
;(.printStackTrace *e)
;





