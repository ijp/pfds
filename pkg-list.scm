(package (pfds (0 3))
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
   "- hamts"
   "- finger trees"
   "- sequences")
  (homepage "http://github.com/ijp/pfds")
  (documentation
   "README.org"
   "LICENSE")
  (libraries
   (sls -> "pfds")
   ("queues" -> ("pdfs" "queues"))
   ("deques" -> ("pdfs" "deques"))
   ("private" -> ("pfds" "private"))))
