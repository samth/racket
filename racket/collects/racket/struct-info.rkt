
(module struct-info "private/kernel.rkt"
  (#%require "private/struct-info.rkt"
             (for-template "private/define-struct.rkt"))
  (#%provide (all-from "private/struct-info.rkt")
             checked-struct-info?))
