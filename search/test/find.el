(setq lexical-binding t)

(load-file "../business/find.el")
(load-file "../int/context.el")

(defvar ag-hit-string
"/Users/fabianhanselmann/repos/rewe-order-service/src/oms/util.clj:176:(defn select-keys-or-nil
/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:2:                              select-keys-or-nil
/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:134:               (select-keys-or-nil [:id :merchant :subOrderValue :deliveryType :channel
/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/order.clj:158:      (select-keys-or-nil [:salutation :title :firstName :lastName :street :addressId
/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/db_to_order.clj:166:      (select-keys-or-nil [:id :lineItemType :productId :articleId :nan :gtin :title :price")

(defvar expected-file-names-and-lines '(("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/util.clj" 176)
                                        ("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj" 53 134)
                                        ("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/order.clj" 158)
                                        ("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/db_to_order.clj" 166)))

(ert-deftest test-find/file-names-and-line-numbers ()
  (let* ((hit-list (reverse (split-string ag-hit-string "\n")))
         (result (find/file-names-and-line-numbers hit-list nil)))
    (should (equal result expected-file-names-and-lines))))

(defvar expected-references '("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:53"
                              "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:134"
                              "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/order.clj:158"
                              "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/db_to_order.clj:3"))

(defvar files-and-lines '(("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj" 53 134)
                          ("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/order.clj" 158)
                          ("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/db_to_order.clj" 3)))

(defvar mocked-file-contents
  '(("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj" . "(ns oms.db.order\n(:require [oms.util :refer :all]))")
    ("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/order.clj" . "(ns oms.business.order\n(:require [oms.util :refer [select-keys-or-nil]]))")
    ("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/db_to_order.clj" . "(ns oms.business.db-to-order\n(:require [oms.util :as u]))\n(defn example [m] (u/select-keys-or-nil m))")))

(defun read-file-content-mock (file-name)
  (alist-get file-name mocked-file-contents "" nil 'equal))

(ert-deftest test-find/ag-result-string->list ()
  (let* ((context (list 'read-file-content #'read-file-content-mock))
         (result (find/ag-result-string->list context files-and-lines "oms.util" "select-keys-or-nil")))
    (should (equal result expected-references))))

(defvar grep-result-list-mock
  '("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:997:                                 (merge (select-keys-or-nil (ff-info-key li)"
    "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:162:      (select-keys-or-nil [:id :lineItemType :productId :articleId :nan :gtin :title :price"
    "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:154:      (select-keys-or-nil [:salutation :title :firstName :lastName :street :addressId"
    "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:130:               (select-keys-or-nil [:id :merchant :subOrderValue :deliveryType :channel"
    "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:47:                              select-keys-or-nil"
    "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/util.clj:176:(defn select-keys-or-nil"))

(defun grep-mock (_) grep-result-list-mock)

(defun ns-name-mock (_) "oms.util")

(defvar expected-references '("/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:53"
                              "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/db/order.clj:134"
                              "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/order.clj:158"
                              "/Users/fabianhanselmann/repos/rewe-order-service/src/oms/business/db_to_order.clj:3"))

(ert-deftest test-find/references ()
  (let* ((context (list 'read-file-content #'read-file-content-mock
                        'grep-project #'grep-mock
                        'ns-name #'ns-name-mock))
         (result (find/references context "select-keys-or-nil")))
    (should (equal result expected-references))))
