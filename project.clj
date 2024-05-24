(defproject lights "0.1.0-SNAPSHOT"
  :description "Changes Hue Lights to randomized color schemes, evolving over time"
  :url "https://github.com/aphyr/lights"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["sonatype-snapshots"
                  {:url "https://oss.sonatype.org/content/repositories/snapshots"
                   :releases false}]]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clhue "0.1.1"]
                 [clj-http "3.12.3"]
                 [clj-wallhack "1.0.1"]
                 [com.evocomputing/colors "1.0.6"]
                 [hickory "0.7.1"]
                 [cheshire "5.12.0"]
                 [org.clojure/tools.cli "1.0.219"]
                 [io.github.zeroone3010/yetanotherhueapi "3.0.0-rc"]
                 [slingshot "0.12.2"]
                 ;[edu.cmu.sphinx/sphinx4-data "1.0-SNAPSHOT"]
                 ;[edu.cmu.sphinx/sphinx4-core "1.0-SNAPSHOT"]
                 ]
  :repl-options {:init-ns user}
  :main ^:skip-aot lights.core
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
