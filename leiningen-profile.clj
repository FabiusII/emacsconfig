{:user {:plugins [;[cider/cider-nrepl "0.22.0-beta9"]
                  [lein-kibit "0.1.7"]
                  [lein-eftest "0.5.8"]]
        :dependencies [[eftest "0.5.8"]
                       [org.clojure/tools.namespace "0.3.1"]]
        :injections [(require 'clojure.pprint)
                     (intern 'clojure.core 'ppr clojure.pprint/pprint)
                     (require 'clojure.tools.namespace.repl)
                     (intern 'clojure.core 'refresh clojure.tools.namespace.repl/refresh)
                     (refer-clojure)]}}

