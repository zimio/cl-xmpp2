(defpackage cl-xmpp2
  (:nicknames xmpp)
  (:use :cl))
(in-package :cl-xmpp2)

(defparameter *doc* (plump:parse "<stream:stream
          from='im.example.com'
          id='++TR84Sm6A3hnt3Q065SnAbbk3Y='
          to='juliet@im.example.com'
          version='1.0'
          xml:lang='en'
          xmlns='jabber:client'
          xmlns:stream='http://etherx.jabber.org/streams'>"))

(defclass connection ()
  ((socket
    :accessor socket)
   (jid
    :initarg :jid
    :accessor jid)
   (username
    :initarg :username
    :accessor username)
   ;; The domain refers to the domain name part of the jid
   (domain
    :initarg :domain
    :accessor domain)
   ;; The socket connects to this hostname
   (hostname
    :accessor hostname
    :initform nil)
   (port
    :accessor port
    :initform nil)
   (connected?
    :accessor connected?
    :initform nil)
   (stream
    :accessor stream
    :initform nil)
   (password
    :initarg :password
    :accessor password)))

(defconstant +xml-doc+ "<?xml version='1.0'?>")
(defconstant +xml-stream+ "<stream:stream from='~a' to='~a' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>")

(defun decode-srv-record (data)
  "Decode a raw SRV record (vector of bytes) into a plist.
DATA is a vector of octets from an SRV DNS response."
  (let ((priority (+ (ash (aref data 0) 8) (aref data 1)))
        (weight   (+ (ash (aref data 2) 8) (aref data 3)))
        (port     (+ (ash (aref data 4) 8) (aref data 5)))
        (i 6)
        (labels '()))
    ;; Parse domain name in DNS label format
    (loop
      with len = (aref data i)
      while (> len 0) do
        (incf i)
        (let ((label (map 'string #'code-char (subseq data i (+ i len)))))
          (push label labels))
        (incf i len)
        (setf len (aref data i)))
    ;; DNS name ends with a 0-length label
    (list :priority priority
          :weight weight
          :port port
          :target (string-downcase (format nil "~{~A~^.~}" (nreverse labels))))))


(defun query-srv (hostname)
  (decode-srv-record (first (org.shirakumo.dns-client:query-data  (concatenate 'string
                                                                               "_xmpp-client._tcp."
                                                                               hostname)
                                                                  :type :SRV))))

(defmethod discover_hostname_port ((conn connection))
  (unless (and (hostname conn)
               (port     conn))
    (let ((answer (query-srv (domain conn))))
      (setf (hostname conn)
            (getf answer :target))
      (setf (port conn)
            (getf answer :port)))))

(defmethod stream_negotiation ((conn connection))
  (write-line +xml-doc+ (stream conn))
  (format (stream conn) +xml-stream+ (jid conn) (domain conn))
  (finish-output (stream conn))
  ;; receive response from server
  ;; return list of supported auth
  )

(defmethod connect_login ((conn connection))
  (restart-case
      (discover_hostname_port conn)
    (set-defaults ()
      :report "Set default hostname and port"
      (setf (port conn) 5222)
      (setf (hostname conn) (domain conn))))
  (setf (socket conn)
        (usocket:socket-connect (hostname conn) (port conn)))
  (setf (stream conn)
        (flexi-streams:make-flexi-stream (usocket:socket-stream (socket conn))
                                         :external-format :utf-16))
  (setf (connected? conn) t)
  )



(defun make-connection (jid password)
  (let ((user/host (uiop:split-string jid :separator "@")))
    (if (= (length user/host) 2)
        (make-instance 'connection
                       :jid jid
                       :password password
                       :username (first user/host)
                       :domain (first (last user/host)))
        (error (format t "Malformed jid couldn't be parsed: ~a" jid)))))
