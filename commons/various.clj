(ns commons.various
	(:use (clojure xml) ))


(defn dissoc-in 
	[ data keys & keys-to-remove ]
		(if (seq keys)
			(reduce #(assoc-in %1 keys 
				(dissoc (get-in %1 keys) %2)) data keys-to-remove)  
			(apply dissoc data keys-to-remove)))

(defmacro update-in-using
	[ data keys ele op & args ]
		`(if-let [s# (get-in ~data ~keys)] 
			(update-in ~data ~keys ~op ~@args)
			(-> ~data (assoc-in ~keys ~ele)
				(update-in ~keys ~op ~@args))))


(defn contains-in?[ m keys ]
	(not (nil? (get-in m keys))))

(defn update-in-get
	[ data keys & op-and-args ]
		(let [new-data (apply update-in data keys op-and-args)
				 new-value (get-in new-data keys) ]
				[new-data new-value]))


