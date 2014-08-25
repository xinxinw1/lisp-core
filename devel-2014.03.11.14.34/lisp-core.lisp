(var mac (mc (name args . body)
           `(var ,name (mc ,args ,@body))))

(mac def (name args . body)
  `(var ,name (fn ,args ,@body)))

(mac block body
  `((fn () ,@body)))

(mac no (a) `(nil? ,a))

(mac when (test . body)
  `(if ,test (do ,@body)))