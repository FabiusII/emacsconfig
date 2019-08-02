{:user {:plugins [[cider/cider-nrepl "0.22.0-beta8"]
                  [lein-kibit "0.1.7"]
                  [lein-eftest "0.5.8"]]
        :dependencies [[eftest "0.5.8"]]
        :injections [(intern 'clojure.core 'ppr 'clojure.pprint/pprint)
                     (intern 'clojure.core 'arrange-requires 'slam.hound/swap-in-reconstructed-ns-form)
                     (refer-clojure)]}}

