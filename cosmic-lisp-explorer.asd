(asdf:defsystem #:cosmic-lisp-explorer
  :description "A 2D roguelike space exploration game in Common Lisp"
  :author "Jayabrota Banerjee jayabrotabanerjee@gmail.com"
  :license "GNU GPL"
  :version "3.0"
  :depends-on (#:cl-charms      ; for terminal interface
               #:cl-raylib      ; for enhanced graphics
               #:alexandria     ; utilities
               #:cl-ppcre       ; string processing
               #:bordeaux-threads ; threading
               #:cl-sdl2        ; sound
               #:cl-fad         ; file operations
               #:lparallel)     ; parallel processing
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main" :depends-on ("package"))
                 
                 (:module "game"
                  :depends-on ("package" "utils")
                  :components
                  ((:file "world")
                   (:file "entities")
                   (:file "player")
                   (:file "combat")
                   (:file "items")
                   (:file "progression")))
                 
                 (:module "ui"
                  :depends-on ("package" "game")
                  :components
                  ((:file "common")
                   (:file "terminal")
                   (:file "enhanced")
                   (:file "sound")))
                 
                 (:module "utils"
                  :depends-on ("package")
                  :components
                  ((:file "procedural")
                   (:file "helpers")
                   (:file "serialization"))))))
  :in-order-to ((test-op (test-op #:cosmic-lisp-explorer/tests))))

(asdf:defsystem #:cosmic-lisp-explorer/tests
  :depends-on (#:cosmic-lisp-explorer
               #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main" :depends-on ("package"))))))
