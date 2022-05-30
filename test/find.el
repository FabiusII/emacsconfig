(setq lexical-binding t)
(load-file "../find.el")

(defvar ag-hit-string
"/Users/fabianhanselmann/repos/rewe-order-service/src/oms/util.clj:176:(defn select-keys-or-nil
/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:53:                              select-keys-or-nil
/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:134:               (select-keys-or-nil [:id :merchant :subOrderValue :deliveryType :channel
/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:158:      (select-keys-or-nil [:salutation :title :firstName :lastName :street :addressId
/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:166:      (select-keys-or-nil [:id :lineItemType :productId :articleId :nan :gtin :title :price")

(defvar ag-hits (list (list "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/util.clj" 176)
                           (list "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj" 53 134 158 166)))

(ert-deftest test-find/file-names-and-line-numbers ()
  (let* ((hit-list (reverse (split-string ag-hit-string "\n")))
         (result (find/file-names-and-line-numbers hit-list nil)))
    (should (equal result ag-hits))))

(defvar expected-references '("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:53"
                              "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:134"
                              "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:158"
                              "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:166"))

(ert-deftest test-find/ag-result-string->list ()
  (let* (result (find/ag-result-string->list ag-hits "oms.util" "select-keys-or-nil"))
    (should (equal result expected-references))))
