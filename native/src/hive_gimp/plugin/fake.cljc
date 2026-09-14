(ns hive-gimp.plugin.fake
  "An in-memory GIMP port, for exercising the dispatch table where there is no
   GIMP: the JVM suite, and the cljrs socket smoke run on the host.

   It is a DOUBLE, and it is held to the real port in exactly one place: the
   live gate (dev/verify_native_plugin.sh) runs the same commands against
   libgimp inside a real GIMP and must see the same result shapes. Nothing
   here is evidence about GIMP's behaviour on its own."
  )

(defn port
  "A fresh fake GIMP. Images and layers share one id counter, as in GIMP."
  []
  (let [state (atom {:next 1 :images [] :image {} :layer {} :saved {}})
        fresh! (fn [] (let [id (:next @state)] (swap! state update :next inc) id))
        img    (fn [id] (or (get-in @state [:image id]) (throw (ex-info (str "no image with id " id) {}))))
        lyr    (fn [id] (or (get-in @state [:layer id]) (throw (ex-info (str "no layer with id " id) {}))))]
    {:state               state
     :version             (fn [] "3.2.4-fake")
     :image-ids           (fn [] (:images @state))
     :image-width         (fn [id] (:width (img id)))
     :image-height        (fn [id] (:height (img id)))
     :image-base-type     (fn [id] (:base (img id)))
     :image-layer-ids     (fn [id] (:layers (img id)))
     :image-file          (fn [id] (:file (img id)))
     :image-new           (fn [w h base]
                            (let [id (fresh!)]
                              (swap! state #(-> %
                                                (assoc-in [:image id] {:width w :height h :base base :layers []})
                                                (update :images (fn [ids] (vec (cons id ids))))))
                              id))
     :image-duplicate     (fn [id]
                            (let [new (fresh!)]
                              (swap! state #(-> %
                                                (assoc-in [:image new] (assoc (img id) :layers []))
                                                (update :images (fn [ids] (vec (cons new ids))))))
                              new))
     :image-flatten       (fn [_] true)
     :image-delete        (fn [id]
                            (img id)
                            (swap! state #(-> %
                                              (update :image dissoc id)
                                              (update :images (fn [ids] (vec (remove #{id} ids))))))
                            true)
     :layer-new           (fn [_ name w h type opacity]
                            (let [id (fresh!)]
                              (swap! state assoc-in [:layer id]
                                     {:name name :width w :height h :alpha? (odd? type) :opacity opacity :fill nil})
                              id))
     :layer-insert        (fn [image layer position]
                            (img image) (lyr layer)
                            (swap! state update-in [:image image :layers]
                                   (fn [ls] (let [[a b] (split-at position ls)] (vec (concat a [layer] b)))))
                            true)
     :layer-name          (fn [id] (:name (lyr id)))
     :layer-visible?      (fn [id] (lyr id) true)
     :layer-opacity       (fn [id] (:opacity (lyr id)))
     :layer-add-alpha     (fn [id] (lyr id) (swap! state assoc-in [:layer id :alpha?] true) true)
     :drawable-width      (fn [id] (:width (lyr id)))
     :drawable-height     (fn [id] (:height (lyr id)))
     :drawable-has-alpha? (fn [id] (:alpha? (lyr id)))
     :fill-color          (fn [id colour] (lyr id) (swap! state assoc-in [:layer id :fill] colour) true)
     :fill-transparent    (fn [id] (lyr id) (swap! state assoc-in [:layer id :fill] :transparent) true)
     :display-new         (fn [_] false)
     :displays-flush      (fn [] true)
     :file-save           (fn [id path] (img id) (swap! state assoc-in [:saved path] id) true)}))
