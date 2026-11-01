;; Usage: PROG=name FUNCS="f g" sbcl --non-interactive --no-userinit --load build.lisp
;; Compiles PROGRAM.lisp with its own declarations, writes the disassembly of
;; each FUNCTION to asm/PROGRAM.asm, and saves bin/PROGRAM as an executable
;; whose toplevel is MAIN. Functions compiled at run time with (compile nil
;; ...) are also disassembled, into asm/PROGRAM-runtime.asm, when the
;; executable runs with SBCL_DISASSEMBLE_RUNTIME set.
(require :sb-concurrency)
(defvar *prog* (sb-ext:posix-getenv "PROG"))
(defvar *funcs* (let ((s (sb-ext:posix-getenv "FUNCS")))
                  (with-input-from-string (in s)
                    (loop for x = (read in nil) while x collect (string x)))))
(ensure-directories-exist "asm/")
(ensure-directories-exist "bin/")
(load (compile-file (format nil "~a.lisp" *prog*)
                    :output-file (format nil "/tmp/samth-sbcl/bin/~a.fasl" *prog*)))
(with-open-file (out (format nil "asm/~a.asm" *prog*) :direction :output :if-exists :supersede)
  (format out ";; ~a, SBCL ~a, compiled with the program's own declarations~%"
          *prog* (lisp-implementation-version))
  (dolist (f *funcs*)
    (let ((sym (find-symbol (string-upcase f))))
      (format out "~%;;; ~a~%" sym)
      (let ((*standard-output* out))
        (disassemble sym)))))
(sb-int:encapsulate 'compile 'capture
  (lambda (real name &optional def)
    (let ((result (funcall real name def)))
      (when (and (null name) (sb-ext:posix-getenv "SBCL_DISASSEMBLE_RUNTIME"))
        (with-open-file (out (format nil "/tmp/samth-sbcl/asm/~a-runtime.asm" *prog*)
                             :direction :output :if-exists :supersede)
          (let ((*standard-output* out))
            (disassemble result))))
      result)))
(sb-ext:save-lisp-and-die (format nil "bin/~a" *prog*)
                          :executable t
                          :toplevel (lambda ()
                                      ;; With SBCL_DISASSEMBLE set, print the hot functions
                                      ;; from this image (same addresses as a timed run).
                                      (if (sb-ext:posix-getenv "SBCL_DISASSEMBLE")
                                          (dolist (f *funcs*)
                                            (format t "~%;;; ~a~%" f)
                                            (disassemble (find-symbol (string-upcase f))))
                                          (main))
                                      (finish-output)
                                      (sb-ext:exit)))
