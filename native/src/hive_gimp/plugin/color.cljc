(ns hive-gimp.plugin.color
  "A colour a client names -> the one spelling GEGL parses without surprises.

   The contract (commands.edn) promises \"CSS name, hex, or rgb() string\" for
   every colour parameter, and the reference plug-in hands that string straight
   to Gegl.Color.new. Measured against GIMP 3.2.4's GEGL, that is wrong twice,
   silently, with the command still answering success:

     \"orange\"        GEGL-WARNING: parsing failed, using transparent cyan
     \"rgb(0,128,0)\"  filled (0,255,0): GEGL reads rgb() channels as 0..1 floats

   So the plug-in normalises here, before GIMP sees the string: hex, CSS rgb()
   and rgba() with 0-255 channels (or percentages), and the 148 CSS named
   colours all become #rrggbb, and anything else is refused by name instead of
   being painted cyan. Portable: clojure.core and clojure.string only."
  (:require [clojure.string :as str]))

(def named
  "CSS Color Module Level 4 named colours."
  {"aliceblue" "#f0f8ff" "antiquewhite" "#faebd7" "aqua" "#00ffff" "aquamarine" "#7fffd4"
   "azure" "#f0ffff" "beige" "#f5f5dc" "bisque" "#ffe4c4" "black" "#000000"
   "blanchedalmond" "#ffebcd" "blue" "#0000ff" "blueviolet" "#8a2be2" "brown" "#a52a2a"
   "burlywood" "#deb887" "cadetblue" "#5f9ea0" "chartreuse" "#7fff00" "chocolate" "#d2691e"
   "coral" "#ff7f50" "cornflowerblue" "#6495ed" "cornsilk" "#fff8dc" "crimson" "#dc143c"
   "cyan" "#00ffff" "darkblue" "#00008b" "darkcyan" "#008b8b" "darkgoldenrod" "#b8860b"
   "darkgray" "#a9a9a9" "darkgreen" "#006400" "darkgrey" "#a9a9a9" "darkkhaki" "#bdb76b"
   "darkmagenta" "#8b008b" "darkolivegreen" "#556b2f" "darkorange" "#ff8c00" "darkorchid" "#9932cc"
   "darkred" "#8b0000" "darksalmon" "#e9967a" "darkseagreen" "#8fbc8f" "darkslateblue" "#483d8b"
   "darkslategray" "#2f4f4f" "darkslategrey" "#2f4f4f" "darkturquoise" "#00ced1" "darkviolet" "#9400d3"
   "deeppink" "#ff1493" "deepskyblue" "#00bfff" "dimgray" "#696969" "dimgrey" "#696969"
   "dodgerblue" "#1e90ff" "firebrick" "#b22222" "floralwhite" "#fffaf0" "forestgreen" "#228b22"
   "fuchsia" "#ff00ff" "gainsboro" "#dcdcdc" "ghostwhite" "#f8f8ff" "gold" "#ffd700"
   "goldenrod" "#daa520" "gray" "#808080" "green" "#008000" "greenyellow" "#adff2f"
   "grey" "#808080" "honeydew" "#f0fff0" "hotpink" "#ff69b4" "indianred" "#cd5c5c"
   "indigo" "#4b0082" "ivory" "#fffff0" "khaki" "#f0e68c" "lavender" "#e6e6fa"
   "lavenderblush" "#fff0f5" "lawngreen" "#7cfc00" "lemonchiffon" "#fffacd" "lightblue" "#add8e6"
   "lightcoral" "#f08080" "lightcyan" "#e0ffff" "lightgoldenrodyellow" "#fafad2" "lightgray" "#d3d3d3"
   "lightgreen" "#90ee90" "lightgrey" "#d3d3d3" "lightpink" "#ffb6c1" "lightsalmon" "#ffa07a"
   "lightseagreen" "#20b2aa" "lightskyblue" "#87cefa" "lightslategray" "#778899" "lightslategrey" "#778899"
   "lightsteelblue" "#b0c4de" "lightyellow" "#ffffe0" "lime" "#00ff00" "limegreen" "#32cd32"
   "linen" "#faf0e6" "magenta" "#ff00ff" "maroon" "#800000" "mediumaquamarine" "#66cdaa"
   "mediumblue" "#0000cd" "mediumorchid" "#ba55d3" "mediumpurple" "#9370db" "mediumseagreen" "#3cb371"
   "mediumslateblue" "#7b68ee" "mediumspringgreen" "#00fa9a" "mediumturquoise" "#48d1cc" "mediumvioletred" "#c71585"
   "midnightblue" "#191970" "mintcream" "#f5fffa" "mistyrose" "#ffe4e1" "moccasin" "#ffe4b5"
   "navajowhite" "#ffdead" "navy" "#000080" "oldlace" "#fdf5e6" "olive" "#808000"
   "olivedrab" "#6b8e23" "orange" "#ffa500" "orangered" "#ff4500" "orchid" "#da70d6"
   "palegoldenrod" "#eee8aa" "palegreen" "#98fb98" "paleturquoise" "#afeeee" "palevioletred" "#db7093"
   "papayawhip" "#ffefd5" "peachpuff" "#ffdab9" "peru" "#cd853f" "pink" "#ffc0cb"
   "plum" "#dda0dd" "powderblue" "#b0e0e6" "purple" "#800080" "rebeccapurple" "#663399"
   "red" "#ff0000" "rosybrown" "#bc8f8f" "royalblue" "#4169e1" "saddlebrown" "#8b4513"
   "salmon" "#fa8072" "sandybrown" "#f4a460" "seagreen" "#2e8b57" "seashell" "#fff5ee"
   "sienna" "#a0522d" "silver" "#c0c0c0" "skyblue" "#87ceeb" "slateblue" "#6a5acd"
   "slategray" "#708090" "slategrey" "#708090" "snow" "#fffafa" "springgreen" "#00ff7f"
   "steelblue" "#4682b4" "tan" "#d2b48c" "teal" "#008080" "thistle" "#d8bfd8"
   "tomato" "#ff6347" "turquoise" "#40e0d0" "violet" "#ee82ee" "wheat" "#f5deb3"
   "white" "#ffffff" "whitesmoke" "#f5f5f5" "yellow" "#ffff00" "yellowgreen" "#9acd32"})

(def ^:private hex-digits "0123456789abcdef")

(defn- byte->hex
  [n]
  (str (subs hex-digits (quot n 16) (inc (quot n 16)))
       (subs hex-digits (rem n 16) (inc (rem n 16)))))

(defn- channel
  "One rgb() argument as 0-255, or nil. `50%` scales and rounds half up; a bare
   number is taken as the CSS 0-255 range and must be whole.

   255 * p / 100, not 2.55 * p: 2.55 has no exact binary spelling, so 50% came
   out 127.4999... and rounded to 127 on all three hosts."
  [token]
  (let [t (str/trim token)]
    (if (str/ends-with? t "%")
      (when-let [p (parse-double (subs t 0 (dec (count t))))]
        (when (<= 0 p 100) (long (+ 0.5 (/ (* 255.0 p) 100.0)))))
      (when-let [n (parse-long t)]
        (when (<= 0 n 255) n)))))

(defn normalize
  "COLOUR -> \"#rrggbb\", or nil when it is not a colour this plug-in will pass
   to GEGL. Case and surrounding whitespace do not matter. Alpha is dropped:
   every caller of this fills an opaque colour."
  [colour]
  (let [c (str/lower-case (str/trim (str colour)))]
    (cond
      (contains? named c) (get named c)

      (re-matches #"#[0-9a-f]{6}" c) c
      (re-matches #"#[0-9a-f]{8}" c) (subs c 0 7)
      (re-matches #"#[0-9a-f]{3}" c)
      (str "#" (apply str (map (fn [d] (str d d)) (subs c 1))))

      (re-matches #"rgba?\((.*)\)" c)
      (let [args (str/split (second (re-matches #"rgba?\((.*)\)" c)) #"[,\s/]+")
            rgb  (map channel (take 3 (remove str/blank? args)))]
        (when (and (= 3 (count rgb)) (every? some? rgb))
          (str "#" (apply str (map byte->hex rgb)))))

      :else nil)))
