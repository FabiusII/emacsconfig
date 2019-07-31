{:user {:plugins [[cider/cider-nrepl "0.22.0-beta8"]
                  [lein-kibit "0.1.7"]
                  [clj-kondo "2019.07.24-alpha"]
                  [lein-eftest "0.5.8"]]
        :dependencies [[eftest "0.5.8"]]
        :injections [(intern 'clojure.core 'ppr 'clojure.pprint/pprint)
                     (refer-clojure)]}}
