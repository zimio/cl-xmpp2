(defsystem "cl-xmpp2"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("plump"
               "lquery"
               "usocket"
               "dns-client"
               "flexi-streams"
               "plump-sexp")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-xmpp2/tests"))))

(defsystem "cl-xmpp2/tests"
  :author ""
  :license ""
  :depends-on ("cl-xmpp2"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-xmpp2"
  :perform (test-op (op c) (symbol-call :rove :run c)))
