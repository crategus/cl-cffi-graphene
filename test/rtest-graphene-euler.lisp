(in-package :graphene-test)

(def-suite graphene-euler :in graphene-suite)
(in-suite graphene-euler)

;;; --- Types and Values -------------------------------------------------------

;;;     graphene_euler_order_t

(test graphene-euler-order-t
  (is (equal '(:DEFAULT
               :XYZ :YZX :ZXY :XZY :YXZ :ZYX
               :SXYZ :SXYX :SXZY :SXZX :SYZX :SYZY
               :SYXZ :SYXY :SZXY :SZXZ :SZYX :SZYZ
               :RZYX :RXYX :RYZX :RXZX :RXZY :RYZY
               :RZXY :RYXY :RYXZ :RZXZ :RXYZ :RZYZ)
             (mapcar (lambda (x)
                       (cffi:foreign-enum-keyword 'graphene:euler-order-t x))
                     '( 0  1  2  3  4  5  6  7  8  9
                       10 11 12 13 14 15 16 17 18 19
                       20 21 22 23 24 25 26 27 28 29
                       30))))
  (is (equal '( 0  1  2  3  4  5  6  7  8  9
               10 11 12 13 14 15 16 17 18 19
               20 21 22 23 24 25 26 27 28 29 30)
             (mapcar (lambda (x)
                       (cffi:foreign-enum-value 'graphene:euler-order-t x))
                     '(:DEFAULT
                       :XYZ :YZX :ZXY :XZY :YXZ :ZYX
                       :SXYZ :SXYX :SXZY :SXZX :SYZX :SYZY
                       :SYXZ :SYXY :SZXY :SZXZ :SZYX :SZYZ
                       :RZYX :RXYX :RYZX :RXZX :RXZY :RYZY
                       :RZXY :RYXY :RYXZ :RZXZ :RXYZ :RZYZ)))))

;;;     graphene_euler_t

;;; --- Macros -----------------------------------------------------------------

(test graphene-with-euler-macro.1
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT EULER 0 0 0)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand '(graphene:with-euler (euler) euler)))))

(test graphene-with-euler-macro.2
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-EULER EULER EULER1)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler euler1) euler)))))

(test graphene-with-euler-macro.3
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-EULER EULER EULER1)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (euler1 graphene:euler-t)) euler)))))

(test graphene-with-euler-macro.4
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-MATRIX EULER MATRIX)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (matrix graphene:matrix-t)) euler)))))

(test graphene-with-euler-macro.5
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-MATRIX EULER MATRIX :DEFAULT)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (matrix graphene:matrix-t) :default)
                  euler)))))

(test graphene-with-euler-macro.6
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-QUATERNION EULER QUAT)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (quat graphene:quaternion-t))
                  euler)))))

(test graphene-with-euler-macro.7
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-QUATERNION EULER QUAT :DEFAULT)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (quat graphene:quaternion-t) :default)
                  euler)))))

(test graphene-with-euler-macro.8
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-VEC3 EULER V)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (v graphene:vec3-t))
                  euler)))))

(test graphene-with-euler-macro.9
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-VEC3 EULER V :DEFAULT)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (v graphene:vec3-t) :default)
                  euler)))))

(test graphene-with-euler-macro.10
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT EULER A B C)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler a b c)
                  euler)))))

(test graphene-with-euler-macro.11
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT EULER A B C ORDER)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler a b c order)
                  euler)))))

(test graphene-with-euler-macro.12
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT EULER A B C ORDER)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (a :deg) b c order)
                  euler)))))

(test graphene-with-euler-macro.13
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-RADIANS EULER A B C)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (a :rad) b c)
                  euler)))))

(test graphene-with-euler-macro.14
  (is (equal '(LET ((EULER (GRAPHENE:EULER-ALLOC)))
                (GRAPHENE:EULER-INIT-FROM-RADIANS EULER A B C ORDER)
                (UNWIND-PROTECT
                  (PROGN EULER)
                  (GRAPHENE:EULER-FREE EULER)))
             (macroexpand
               '(graphene:with-euler (euler (a :rad) b c order)
                  euler)))))

;; no args
(test graphene-with-euler.1
  (graphene:with-euler (euler)
    (is (cffi:pointerp euler))
    (is (= 0 (graphene:euler-x euler)))
    (is (= 0 (graphene:euler-y euler)))
    (is (= 0 (graphene:euler-z euler)))
    (is (eq :zyx (graphene:euler-order euler)))))

;; three angles in degrees
(test graphene-with-euler.2
  (graphene:with-euler (euler 1/2 2 4.0)
    (is (cffi:pointerp euler))
    (is (approx-equal 0.5 (graphene:euler-x euler)))
    (is (approx-equal 2.0 (graphene:euler-y euler)))
    (is (approx-equal 4.0 (graphene:euler-z euler)))
    (is (eq :zyx (graphene:euler-order euler))))
  (graphene:with-euler (euler (1/2 :deg) 2 4.0)
    (is (cffi:pointerp euler))
    (is (approx-equal 0.5 (graphene:euler-x euler)))
    (is (approx-equal 2.0 (graphene:euler-y euler)))
    (is (approx-equal 4.0 (graphene:euler-z euler)))
    (is (eq :zyx (graphene:euler-order euler)))))

;; three angles in degrees and an order
(test graphene-with-euler.3
  (graphene:with-euler (euler 1/2 2 4.0 :SXYZ)
    (is (cffi:pointerp euler))
    (is (approx-equal 0.5 (graphene:euler-x euler)))
    (is (approx-equal 2.0 (graphene:euler-y euler)))
    (is (approx-equal 4.0 (graphene:euler-z euler)))
    (is (eq :sxyz (graphene:euler-order euler))))
  (graphene:with-euler (euler (1/2 :deg) 2 4.0 :SXYZ)
    (is (cffi:pointerp euler))
    (is (approx-equal 0.5 (graphene:euler-x euler)))
    (is (approx-equal 2.0 (graphene:euler-y euler)))
    (is (approx-equal 4.0 (graphene:euler-z euler)))
    (is (eq :sxyz (graphene:euler-order euler)))))

;; three angles in radians and with optional order
(test graphene-with-euler.4
  (graphene:with-euler (euler ((/ pi 2) :rad) pi (* 3 (/ pi 2)))
    (is (cffi:pointerp euler))
    (is (approx-equal  90.0 (graphene:euler-x euler)))
    (is (approx-equal 180.0 (graphene:euler-y euler)))
    (is (approx-equal 270.0 (graphene:euler-z euler)))
    (is (eq :zyx (graphene:euler-order euler))))
  (graphene:with-euler (euler ((/ pi 2) :rad) pi (* 3 (/ pi 2)) :sxyz)
    (is (cffi:pointerp euler))
    (is (approx-equal  90.0 (graphene:euler-x euler)))
    (is (approx-equal 180.0 (graphene:euler-y euler)))
    (is (approx-equal 270.0 (graphene:euler-z euler)))
    (is (eq :sxyz (graphene:euler-order euler)))))

(test graphene-with-euler.5
  (graphene:with-matrix (mat)
    ;; Initialize from matrix
    (graphene:with-euler (euler (mat graphene:matrix-t))
      (is (cffi:pointerp euler))
      (is (approx-equal 0.0 (graphene:euler-x euler)))
      (is (approx-equal 0.0 (graphene:euler-y euler)))
      (is (approx-equal 0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler))))
    ;; Initialize from matrix with order
    (graphene:with-euler (euler (mat graphene:matrix-t) :sxyz)
      (is (cffi:pointerp euler))
      (is (approx-equal 0.0 (graphene:euler-x euler)))
      (is (approx-equal 0.0 (graphene:euler-y euler)))
      (is (approx-equal 0.0 (graphene:euler-z euler)))
      (is (eq :sxyz (graphene:euler-order euler))))))

(test graphene-with-euler.6
  (graphene:with-quaternion (quat 90 0 0)
    ;; Initialize from quaternion
    (graphene:with-euler (euler (quat graphene:quaternion-t))
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler))))
    ;; Initialize from quaternion with order
    (graphene:with-euler (euler (quat graphene:quaternion-t) :sxyz)
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :sxyz (graphene:euler-order euler))))))

(test graphene-with-euler.7
  (graphene:with-vec3 (vec 90 0 0)
    ;; Initialize from vector
    (graphene:with-euler (euler (vec graphene:vec3-t))
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler))))
    ;; Initialize from vector with order
    (graphene:with-euler (euler (vec graphene:vec3-t) :sxyz)
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :sxyz (graphene:euler-order euler))))))

(test graphene-with-euler.8
  (graphene:with-eulers (euler (euler1 euler) (euler2 (euler graphene:euler-t)))
    (is (graphene:euler-equal euler euler1))
    (is (graphene:euler-equal euler euler2))
    (is (graphene:euler-equal euler1 euler2))))

;;; --- Functions --------------------------------------------------------------

;;;     graphene_euler_alloc
;;;     graphene_euler_free

(test graphene-euler-alloc/free
  (let (euler)
    (is (cffi:pointerp (setf euler (graphene:euler-alloc))))
    (is (not (cffi:null-pointer-p euler)))
    (is-false (graphene:euler-free euler))))

;;;     graphene_euler_init

(test graphene-euler-init
  (let ((euler (graphene:euler-alloc)))
    (is (cffi:pointerp (setf euler (graphene:euler-init euler 0 0 0))))
    (is (= 0 (graphene:euler-x euler)))
    (is (= 0 (graphene:euler-y euler)))
    (is (= 0 (graphene:euler-z euler)))
    (is (cffi:pointerp (setf euler (graphene:euler-init euler 1/2 2.0 5))))
    (is (approx-equal 0.5 (graphene:euler-x euler)))
    (is (approx-equal 2.0 (graphene:euler-y euler)))
    (is (approx-equal 5.0 (graphene:euler-z euler)))
    ;; Initialization with an order
    (is (cffi:pointerp (graphene:euler-init euler 90 0 0 :default)))
    (is (eq :zyx (graphene:euler-order euler)))
    (is (approx-equal 90.0 (graphene:euler-x euler)))
    (is (approx-equal  0.0 (graphene:euler-y euler)))
    (is (approx-equal  0.0 (graphene:euler-z euler)))
    (is (cffi:pointerp (graphene:euler-init euler 0 90 0 :sxyz)))
    (is (eq :sxyz (graphene:euler-order euler)))
    (is (approx-equal  0.0 (graphene:euler-x euler)))
    (is (approx-equal 90.0 (graphene:euler-y euler)))
    (is (approx-equal  0.0 (graphene:euler-z euler)))
    (is (cffi:pointerp (graphene:euler-init euler 0 0 90 :syzx)))
    (is (eq :syzx (graphene:euler-order euler)))
    (is (approx-equal  0.0 (graphene:euler-x euler)))
    (is (approx-equal  0.0 (graphene:euler-y euler)))
    (is (approx-equal 90.0 (graphene:euler-z euler)))
    (is (cffi:pointerp (graphene:euler-init euler 90 90 0 :szyx)))
    (is (eq :szyx (graphene:euler-order euler)))
    (is (approx-equal 90.0 (graphene:euler-x euler)))
    (is (approx-equal 90.0 (graphene:euler-y euler)))
    (is (approx-equal  0.0 (graphene:euler-z euler)))
    ;; Free the allocated resources
    (is-false (graphene:euler-free euler))))

;;;     graphene_euler_init_with_order

;; not implemented, included in graphene:euler-init

;;;     graphene_euler_init_from_matrix

(test graphene-euler-init-from-matrix
  (graphene:with-euler (euler)
    (graphene:with-matrix (mat)
      ;; Initialize from identity matrix
      (is (cffi:pointerp (graphene:euler-init-from-matrix euler mat)))
      (is (approx-equal 0.0 (graphene:euler-x euler)))
      (is (approx-equal 0.0 (graphene:euler-y euler)))
      (is (approx-equal 0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler)))
      ;; Initialize with nil
      (is (cffi:pointerp (graphene:euler-init-from-matrix euler nil)))
      (is (= 0.0 (graphene:euler-x euler)))
      (is (= 0.0 (graphene:euler-y euler)))
      (is (= 0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler)))
      ;; Initialize from rotation matrix
      (is (cffi:pointerp
              (graphene:matrix-init-from-float mat
                                               1.0  0.0  0.0  0.0
                                               0.0  0.0  1.0  0.0
                                               0.0 -1.0  0.0  0.0
                                               0.0  0.0  0.0  1.0)))
      (is (cffi:pointerp (graphene:euler-init-from-matrix euler mat)))
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler)))
      ;; Initialize from rotation matrix and order
      (is (cffi:pointerp
              (graphene:matrix-init-from-float mat
                                               1.0  0.0  0.0  0.0
                                               0.0  0.0  1.0  0.0
                                               0.0 -1.0  0.0  0.0
                                               0.0  0.0  0.0  1.0)))
      (is (cffi:pointerp (graphene:euler-init-from-matrix euler mat :sxyz)))
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :sxyz (graphene:euler-order euler))))))

;;;     graphene_euler_init_from_quaternion

(test graphene-euler-init-from-quaternion
  (graphene:with-euler (euler)
    (graphene:with-quaternion (quat 90 0 0)
      ;; Init from quaternion
      (is (cffi:pointerp (graphene:euler-init-from-quaternion euler quat)))
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler)))
      ;; Init from nil
      (is (cffi:pointerp (graphene:euler-init-from-quaternion euler nil)))
      (is (= 0.0 (graphene:euler-x euler)))
      (is (= 0.0 (graphene:euler-y euler)))
      (is (= 0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler)))
      ;; Init from quaternion with order
      (is (cffi:pointerp (graphene:euler-init-from-quaternion euler quat :sxyz)))
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :sxyz (graphene:euler-order euler))))))

;;;     graphene_euler_init_from_vec3

(test graphene-euler-init-from-vec3
  (graphene:with-euler (euler)
    (graphene:with-vec3 (vec 90 0 0)
      ;; Initialize from vector
      (is (cffi:pointerp (graphene:euler-init-from-vec3 euler vec)))
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler)))
      ;; Initialize from nil
      (is (cffi:pointerp (graphene:euler-init-from-vec3 euler nil)))
      (is (= 0.0 (graphene:euler-x euler)))
      (is (= 0.0 (graphene:euler-y euler)))
      (is (= 0.0 (graphene:euler-z euler)))
      (is (eq :zyx (graphene:euler-order euler)))
      ;; Initialize from vector with order
      (is (cffi:pointerp (graphene:euler-init-from-vec3 euler vec :sxyz)))
      (is (approx-equal 90.0 (graphene:euler-x euler)))
      (is (approx-equal  0.0 (graphene:euler-y euler)))
      (is (approx-equal  0.0 (graphene:euler-z euler)))
      (is (eq :sxyz (graphene:euler-order euler))))))

;;;     graphene_euler_init_from_euler
;;;     graphene_euler_equal

(test graphene-euler-init-from-euler
  (graphene:with-eulers (euler1 euler2)
    (is (cffi:pointerp (graphene:euler-init euler1 90 180 270 :sxyz)))
    ;; Initialize from another euler instance
    (is (cffi:pointerp (graphene:euler-init-from-euler euler2 euler1)))
    (is (graphene:euler-equal euler1 euler2))
    (is (approx-equal  90.0 (graphene:euler-x euler2)))
    (is (approx-equal 180.0 (graphene:euler-y euler2)))
    (is (approx-equal 270.0 (graphene:euler-z euler2)))
    (is (eq :sxyz (graphene:euler-order euler2)))
    ;; Initialize from nil
    (is (cffi:pointerp (graphene:euler-init-from-euler euler2 nil)))
    (is (approx-equal 0.0 (graphene:euler-x euler2)))
    (is (approx-equal 0.0 (graphene:euler-y euler2)))
    (is (approx-equal 0.0 (graphene:euler-z euler2)))
    (is (eq :zyx (graphene:euler-order euler2)))))

;;;     graphene_euler_init_from_radians

(test graphene-euler-init-from-radians
  (graphene:with-euler (euler)
    (is (cffi:pointerp
          (graphene:euler-init-from-radians euler (/ pi 2)
                                                  pi
                                                  (* 3 (/ pi 2))
                                                  :default)))
      (is (approx-equal  90.0 (graphene:euler-x euler)))
      (is (approx-equal 180.0 (graphene:euler-y euler)))
      (is (approx-equal 270.0 (graphene:euler-z euler)))))

;;;     graphene_euler_get_x
;;;     graphene_euler_get_y
;;;     graphene_euler_get_z

(test graphene-euler-x/y/z
  (graphene:with-euler (euler)
    (is (cffi:pointerp (setf euler (graphene:euler-init euler 1/2 2 3.0))))
    (is (typep (graphene:euler-x euler) 'single-float))
    (is (approx-equal 0.5 (graphene:euler-x euler)))
    (is (typep (graphene:euler-y euler) 'single-float))
    (is (approx-equal 2.0 (graphene:euler-y euler)))
    (is (typep (graphene:euler-z euler) 'single-float))
    (is (approx-equal 3.0 (graphene:euler-z euler)))))

;;;     graphene_euler_get_order

(test graphene-euler-order
  (graphene:with-euler (euler)
    (is (eq :zyx (graphene:euler-order euler))))
  (graphene:with-euler (euler 90 180 270 :sxyz)
    (is (eq :sxyz (graphene:euler-order euler)))))

;;;     graphene_euler_get_alpha
;;;     graphene_euler_get_beta
;;;     graphene_euler_get_gamma

(test graphene-euler-alpha/beta/gamma
  (graphene:with-euler (euler 90 180 270)
    (is (typep (graphene:euler-alpha euler) 'single-float))
    (is (approx-equal (/ pi 2) (graphene:euler-alpha euler)))
    (is (typep (graphene:euler-beta euler) 'single-float))
    (is (approx-equal pi (graphene:euler-beta euler)))
    (is (typep (graphene:euler-gamma euler) 'single-float))
    (is (approx-equal (* 3 (/ pi 2)) (graphene:euler-gamma euler)))))

;;;     graphene_euler_to_vec3

(test graphene-euler-to-vec3
  (graphene:with-euler (euler 90 180 270)
    (graphene:with-vec3 (vec)
      (is (cffi:pointerp (graphene:euler-to-vec3 euler vec)))
      (is (every #'approx-equal '(90.0 180.0 270.0)
                                (graphene:vec3-to-float vec))))))

;;;     graphene_euler_to_matrix

(test graphene-euler-to-matrix
  (graphene:with-euler (euler 90 0 0)
    (graphene:with-matrix (matrix)
      (is (cffi:pointerp (graphene:euler-to-matrix euler matrix)))
      (is (every #'approx-equal
                 '(1.0  0.0  0.0  0.0
                   0.0  0.0  1.0  0.0
                   0.0 -1.0  0.0  0.0
                   0.0  0.0  0.0  1.0)
                 (graphene:matrix-to-float matrix)))
      (is (cffi:pointerp (graphene:euler-init euler 0 90 0)))
      (is (cffi:pointerp (graphene:euler-to-matrix euler matrix)))
      (is (every #'approx-equal
                 '(0.0  0.0 -1.0  0.0
                   0.0  1.0  0.0  0.0
                   1.0  0.0  0.0  0.0
                   0.0  0.0  0.0  1.0)
                 (graphene:matrix-to-float matrix)))
      (is (cffi:pointerp (graphene:euler-init euler 0 0 90)))
      (is (cffi:pointerp (graphene:euler-to-matrix euler matrix)))
      (is (every #'approx-equal
                 '(0.0  1.0  0.0  0.0
                  -1.0  0.0  0.0  0.0
                   0.0  0.0  1.0  0.0
                   0.0  0.0  0.0  1.0)
                 (graphene:matrix-to-float matrix))))))

;;;     graphene_euler_to_quaternion

(test graphene-euler-to-quaternion
  (graphene:with-euler (euler 90 0 0)
    (graphene:with-quaternion (quat)
      (is (cffi:pointerp (graphene:euler-to-quaternion euler quat)))
      (is (every #'approx-equal
                 '(90.0 0.0 0.0)
                 (multiple-value-list (graphene:quaternion-to-angles quat))))
      (is (cffi:pointerp (graphene:euler-init euler 0 90 0)))
      (is (cffi:pointerp (graphene:euler-to-quaternion euler quat)))
      ;; The precision for this case is worse. Why?
      (is (every #'approx-equal
                 '(0.0 90.0 0.0)
                 (multiple-value-list (graphene:quaternion-to-angles quat))
                 '(1.0e-5 1.0e-3 1.0e-5)))
      (is (cffi:pointerp (graphene:euler-init euler 0 0 90)))
      (is (cffi:pointerp (graphene:euler-to-quaternion euler quat)))
      (is (every #'approx-equal
                 '(0.0 0.0 90.0)
                 (multiple-value-list (graphene:quaternion-to-angles quat)))))))

;;;     graphene_euler_reorder

(test graphene-euler-reorder
  (graphene:with-eulers ((euler 90 0 0) result)
    (is (cffi:pointerp (graphene:euler-reorder euler :sxyz result)))
    (is (approx-equal 90.0 (graphene:euler-x result)))
    (is (approx-equal  0.0 (graphene:euler-y result)))
    (is (approx-equal  0.0 (graphene:euler-z result)))
    (is (cffi:pointerp (graphene:euler-reorder euler :syzx result)))
    (is (approx-equal -90.0 (graphene:euler-x result)))
    (is (approx-equal  90.0 (graphene:euler-y result)))
    (is (approx-equal  90.0 (graphene:euler-z result)))
    (is (cffi:pointerp (graphene:euler-reorder euler :szyx result)))
    (is (approx-equal -90.0 (graphene:euler-x result)))
    (is (approx-equal -90.0 (graphene:euler-y result)))
    (is (approx-equal  90.0 (graphene:euler-z result)))))

;;; 2024-9-9
