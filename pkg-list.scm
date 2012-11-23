(package (pfds (0 2))
  (depends (wak-trc-testing))
  (synopsis "Purely Functional Data Structures")
  (description
   "A library of data structures for functional programmers."
   "It contains implementations of:"
   "- queues"
   "- deques"
   "- bbtrees"
   "- sets"
   "- dlists"
   "- priority search queues"
   "- heaps"
   "- finger trees"
   "- sequences")
  (homepage "http://github.com/ijp/pfds")
  (documentation
   "README.org"
   "LICENSE")
  (libraries
   (sls -> "pfds")
   ("private" -> ("pfds" "private"))))
