(use 'ClojureWpf.core)

(def sandbox (dev-sandbox))
(def wind (:window sandbox))
(at wind :Content (caml :StackPanel [:Button#btn "Click me"] [:TextBlock#txt "Not clicked"]))
(defattached click-count)
(at wind click-count (atom 0))
(defn onclick [s e]
  (when-let [cnt @click-count]
    (swap! cnt inc)
    (at [wind :txt] :Text (str "Clicked " @cnt " times"))))
(at [wind :btn] :Click onclick)