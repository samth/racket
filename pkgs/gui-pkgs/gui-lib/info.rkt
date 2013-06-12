#lang setup/infotab

(define deps '("draw-lib"
               "snip"
               "wxme"
               "pict"
               "scribble-lib"
               "string-constants"
               "unstable" ; for class-iop
               ("gui-i386-macosx" #:platform "i386-macosx")
               ("gui-x86_64-macosx" #:platform "x86_64-macosx")
               ("gui-win32-i386" #:platform "win32\\i386")
               ("gui-win32-x86_64" #:platform "win32\\x86_64")))
