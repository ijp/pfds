(package (pfds (0))
  (depends (wak-trc-testing))
  (synopsis "Purely Functional Data Structures")
  (description
   "A library of data structures for functional programmers (eventually)."
   "Right now just queues and deques.")
  (homepage "http://github.com/ijp/pfds")
  (libraries
   (sls -> "pfds")
   ("private" -> ("pfds" "private")))
  (programs
   (("tests.scm") -> "pfds-tests.scm")))
