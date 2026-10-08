(ns hive-gimp.pdb.starter
  "The PDB procedures hive-gimp ships native, from
   `resources/hive_gimp/pdb/starter.edn`: a filter plug-in
   (`plug-in-despeckle`), a drawable operation (`gimp-drawable-invert`) and a
   file export (`file-png-export`). Loading this namespace registers them."
  (:require [hive-gimp.pdb.native :refer [defprocedures]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def commands
  "The catalogue commands this namespace registered."
  (defprocedures "hive_gimp/pdb/starter.edn"))
