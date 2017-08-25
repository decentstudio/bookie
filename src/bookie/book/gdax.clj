(ns bookie.book.gdax
  (:require [keychain.exchange.gdax :as gdax]
            [clojure.core.async :as a]))

(defn ->bids-xf
  [sequence-number]
  (map
    (fn [[price size id :as order]]
      {:product_id "ETH-USD"
       :type "open"
       :side "buy"
       :sequence sequence-number
       :price (read-string price)
       :remaining_size (read-string size)
       :order_id id})))

(defn ->asks-xf
  [sequence-number]
  (map
    (fn [[price size id :as order]]
      {:product_id "ETH-USD"
       :type "open"
       :side "sell"
       :sequence sequence-number
       :price (read-string price)
       :remaining_size (read-string size)
       :order_id id})))

(defn add-event
  [book {:keys [sequence type side product_id order_id] :as order}]
  (case type
    "match" (swap! book update-in [product_id :matches] #(conj % order))
    (swap! book update-in [product_id side order_id] #(conj % order))))

(defn start-order-book-maintainer
  [order-book subscription start-sequence add-fn]
  (let [feed (:feed subscription)]
    (a/go-loop
      [[timestamp event :as m] (a/<! feed)
       previous-sequence start-sequence]
      (when m
        (let [current-sequence (:sequence event)]
          (when (> current-sequence previous-sequence)
              (add-fn order-book event))
          (recur (a/<! feed) current-sequence))))))

(defn load-snapshot
  [book {:keys [sequence bids asks] :as snapshot} add-fn]
  (let [bids-xf (->bids-xf sequence)
        asks-xf (->asks-xf sequence)]
    (doseq [order (flatten [(transduce bids-xf conj bids)
                            (transduce asks-xf conj asks)])]
      (add-fn book order))))

(defn get-snapshot
  [client product]
  (gdax/get-product-order-book client product :level 3))

(defn create-order-book
  [product]
  (let [order-book (atom {})
        subscription (gdax/subscribe [product])
        snapshot (get-snapshot (gdax/get-client) product)
        _ (load-snapshot order-book snapshot add-event)
        _ (start-order-book-maintainer order-book subscription (:sequence snapshot) add-event)
        errored (a/chan)
        closed (a/chan)]
    {:order-book order-book
     :errored (a/pipe (:errored subscription) errored)
     :closed (a/pipe (:closed subscription) closed)
     :close (:close subscription)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Snapshot Operations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn open?
  [order]
  (= "open" (-> order val first :type)))

(defn buy?
  [order]
  (= "buy" (-> order val first :side)))

(defn sell?
  [order]
  (= "sell" (-> order val first :side)))

(defn open-x-order?*
  "General predicate for open sell/buy order"
  [pred-fn order]
  (and (open? order)
       (pred-fn order)))

(def open-buy-order?
  (partial open-x-order?* buy?))

(def open-sell-order?
  (partial open-x-order?* sell?))

(defn B
  "Open bid limit orders."
  [snapshot product]
  (filter open? (get-in snapshot [product "buy"])))

(defn A
  "Open ask limit orders."
  [snapshot product]
  (filter open? (get-in snapshot [product "sell"])))

(defn get-price
  [order]
  (->> order
       val
       first
       :price))

(defn get-prices
  [orders]
  (map get-price orders))

(defn b
  "Current bid price."
  [snapshot product]
  (->> (B snapshot product)
       (get-prices)
       (apply max)))

(defn a
  "Current ask price."
  [snapshot product]
  (->> (A snapshot product)
       (get-prices)
       (apply min)))

(defn s
  "Current bid-ask spread."
  [snapshot product]
  (- (a snapshot product) (b snapshot product)))

(defn m
  "Current mid price."
  [snapshot product]
  (/ (+ (a snapshot product) (b snapshot product)) 2))

(defn nxpt*
  "General function for nbpt and napt."
  [side-fn snapshot product price]
  (let [xf (comp (filter #(= price (get-price %)))
                 (map val)
                 (map first)
                 (map #(or (:size %) (:remaining_size %))))]
    (transduce xf + (side-fn snapshot product))))

(defn nbpt
  "Current bid-side depth at a given price."
  [snapshot product price]
  (nxpt* B snapshot product price))

(defn napt
  "Current ask-side depth at a given price."
  [snapshot product price]
  (nxpt* A snapshot product price))

(defn nxpt-profile*
  "General function for nbpt-profile and napt-profile."
  [side-fn snapshot product]
  (->> (side-fn snapshot product)
       (get-prices)
       (into #{})
       (pmap (fn [p] {p (nxpt* side-fn snapshot product p)}))
       (reduce merge)))

(defn nbpt-profile
  "Current bid-side depth profile for a given snapshot."
  [snapshot product]
  (nxpt-profile* B snapshot product))

(defn napt-profile
  "Current ask-side depth profile for a given snapshot."
  [snapshot product]
  (nxpt-profile* A snapshot product))
