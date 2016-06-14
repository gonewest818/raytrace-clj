(defproject raytrace-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/imagez "0.10.0"]
                 [net.mikera/core.matrix "0.52.0"]
                 [net.mikera/vectorz-clj "0.44.0"]
                 [com.climate/claypoole "1.1.2"]
                 [metrics-clojure "2.7.0"]
                 [org.slf4j/slf4j-log4j12 "1.7.21"]
                 [log4j "1.2.17" :exclusions [javax.mail/mail
                                                javax.jms/jms
                                                com.sun.jdmk/jmxtools
                                                com.sun.jmx/jmxri]]]
                 ;[com.taoensso/timbre "4.3.1"]
  :main ^:skip-aot raytrace-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
