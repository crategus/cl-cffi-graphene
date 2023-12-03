(in-package :graphene-test)

(def-suite graphene-utils :in graphene-suite)
(in-suite graphene-utils)

;;; --- graphene:with-object for points ----------------------------------------

(test with-object-point.1
  (graphene:with-object (p graphene:point-t)
    (is (= 0.0 (graphene:point-x p)))
    (is (= 0.0 (graphene:point-y p)))))

(test with-object-point.2
  (graphene:with-object (p graphene:point-t 1 2)
    (is (= 1.0 (graphene:point-x p)))
    (is (= 2.0 (graphene:point-y p)))))

(test with-object-point.3
  (graphene:with-objects ((p1 graphene:point-t 3 4) (p graphene:point-t p1))
    (is (= 3.0 (graphene:point-x p)))
    (is (= 4.0 (graphene:point-y p)))))

(test with-object-point.4
  (graphene:with-objects ((p1 graphene:point-t 5 6)
                          (p graphene:point-t (p1 graphene:point-t)))
    (is (= 5.0 (graphene:point-x p)))
    (is (= 6.0 (graphene:point-y p)))))

(test with-object-point.5
  (graphene:with-objects ((v graphene:vec2-t 1.5 2.5)
                          (p graphene:point-t (v graphene:vec2-t)))
    (is (= 1.5 (graphene:point-x p)))
    (is (= 2.5 (graphene:point-y p)))))

(test with-object-point.6
  (let ((a 3.5) (b 4.5))
    (graphene:with-object (p graphene:point-t a b)
      (is (= 3.5 (graphene:point-x p)))
      (is (= 4.5 (graphene:point-y p))))))

(test with-objects-points.1
  (graphene:with-objects ((p1 graphene:point-t)
                          (p2 graphene:point-t)
                          (p3 graphene:point-t))
    (is (graphene:point-equal p1 (graphene:point-zero)))
    (is (graphene:point-equal p2 (graphene:point-zero)))
    (is (graphene:point-equal p3 (graphene:point-zero)))))

(test with-objects-points.2
  (graphene:with-objects ((p1 graphene:point-t)
                          (p2 graphene:point-t 1 2)
                          (p3 graphene:point-t p2)
                          (p4 graphene:point-t (p3 graphene:point-t)))
    (is (graphene:point-equal p1 (graphene:point-zero)))
    (is (= 1.0 (graphene:point-x p2)))
    (is (= 2.0 (graphene:point-y p2)))
    (is (graphene:point-equal p3 p2))
    (is (graphene:point-equal p4 p3))))

(test with-objects-points.3
  (graphene:with-objects ((v graphene:vec2-t 1.5 2.5)
                          (p1 graphene:point-t (v graphene:vec2-t))
                          (p2 graphene:point-t)
                          (p3 graphene:point-t p1))
    (is (= 1.5 (graphene:point-x p1)))
    (is (= 2.5 (graphene:point-y p1)))
    (is (graphene:point-equal p2 (graphene:point-zero)))
    (is (graphene:point-equal p3 p1))))

;;; --- 2023-12-3 --------------------------------------------------------------

