(ns hive-gimp.plugin.fake
  "An in-memory GIMP port, for exercising the dispatch table where there is no
   GIMP: the JVM suite, the portability gate, and the cljrs socket smoke run.

   It is a DOUBLE, and it is held to the real port in two places:
   plugin-test/the-fake-real-and-declared-ports-agree checks that it names the
   same functions as the port main.cljrs builds from gimp.native/*, and the live
   gate (dev/verify_native_plugin.sh) runs the commands against libgimp inside a
   real GIMP. Nothing here is evidence about GIMP's behaviour on its own.")

(defn- rotate-dims [{:keys [width height] :as m} kind]
  (if (= 1 kind) m (assoc m :width height :height width)))

(defn port
  "A fresh fake GIMP. Images and layers share one id counter, as in GIMP."
  []
  (let [state  (atom {:next 1 :images [] :image {} :layer {} :saved {}})
        fresh! (fn [] (let [id (:next @state)] (swap! state update :next inc) id))
        img    (fn [id] (or (get-in @state [:image id]) (throw (ex-info (str "no image with id " id) {}))))
        lyr    (fn [id] (or (get-in @state [:layer id]) (throw (ex-info (str "no layer with id " id) {}))))
        add-image! (fn [m]
                     (let [id (fresh!)]
                       (swap! state #(-> %
                                         (assoc-in [:image id] m)
                                         (update :images (fn [ids] (vec (cons id ids))))))
                       id))]
    {:state               state
     :version             (fn [] "3.2.4-fake")
     :image-ids           (fn [] (:images @state))
     :image-width         (fn [id] (:width (img id)))
     :image-height        (fn [id] (:height (img id)))
     :image-base-type     (fn [id] (:base (img id)))
     :image-layer-ids     (fn [id] (:layers (img id)))
     :image-file          (fn [id] (:file (img id)))
     :image-new           (fn [w h base] (add-image! {:width w :height h :base base :layers []}))
     :image-duplicate     (fn [id] (add-image! (assoc (img id) :layers [])))
     :image-flatten       (fn [id]
                            (let [layers (:layers (img id))]
                              (swap! state assoc-in [:image id :layers] (vec (take 1 layers)))
                              (boolean (seq layers))))
     :image-delete        (fn [id]
                            (img id)
                            (swap! state #(-> %
                                              (update :image dissoc id)
                                              (update :images (fn [ids] (vec (remove #{id} ids))))))
                            true)
     :image-scale         (fn [id w h] (img id) (swap! state update-in [:image id] assoc :width w :height h) true)
     :image-crop          (fn [id w h _ _] (img id) (swap! state update-in [:image id] assoc :width w :height h) true)
     :image-rotate        (fn [id kind] (img id) (swap! state update-in [:image id] rotate-dims kind) true)
     :image-flip          (fn [id _] (img id) true)
     :image-remove-layer  (fn [image layer]
                            (img image) (lyr layer)
                            (swap! state update-in [:image image :layers] (fn [ls] (vec (remove #{layer} ls))))
                            true)
     :file-load           (fn [path] (add-image! {:width 64 :height 64 :base 0 :layers [] :file path}))
     :layer-new           (fn [_ name w h type opacity]
                            (let [id (fresh!)]
                              (swap! state assoc-in [:layer id]
                                     {:name name :width w :height h :alpha? (odd? type) :opacity opacity
                                      :visible? true :fill nil})
                              id))
     :layer-copy          (fn [id]
                            (let [copy (fresh!)
                                  l    (lyr id)]
                              (swap! state assoc-in [:layer copy] (update l :name str " copy"))
                              copy))
     :layer-insert        (fn [image layer position]
                            (img image) (lyr layer)
                            (swap! state update-in [:image image :layers]
                                   (fn [ls] (let [[a b] (split-at position ls)] (vec (concat a [layer] b)))))
                            true)
     :layer-name          (fn [id] (:name (lyr id)))
     :layer-visible?      (fn [id] (:visible? (lyr id)))
     :layer-opacity       (fn [id] (:opacity (lyr id)))
     :layer-add-alpha     (fn [id] (lyr id) (swap! state assoc-in [:layer id :alpha?] true) true)
     :layer-set-opacity   (fn [id o] (lyr id) (swap! state assoc-in [:layer id :opacity] o) true)
     :item-set-name       (fn [id n] (lyr id) (swap! state assoc-in [:layer id :name] n) true)
     :item-set-visible    (fn [id v] (lyr id) (swap! state assoc-in [:layer id :visible?] v) true)
     :drawable-width      (fn [id] (:width (lyr id)))
     :drawable-height     (fn [id] (:height (lyr id)))
     :drawable-has-alpha? (fn [id] (:alpha? (lyr id)))
     :fill-color          (fn [id colour] (lyr id) (swap! state assoc-in [:layer id :fill] colour) true)
     :fill-transparent    (fn [id] (lyr id) (swap! state assoc-in [:layer id :fill] :transparent) true)
     :display-new         (fn [_] false)
     :displays-flush      (fn [] true)
     :file-save           (fn [id path] (img id) (swap! state assoc-in [:saved path] id) true)}))
