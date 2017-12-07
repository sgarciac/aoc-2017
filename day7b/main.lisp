(in-package #:day7b)

(let ((entries (read-input "input")))
  (find-first-correction (aoc-tree-from-entries (find-base-name (parents entries)) entries)))

