(in-package #:brine-shrimp-sim)

(defstruct pos x y z)
(defstruct velocity x y z)

(defsystem pos-system (pos))

(defmethod update-system ((world world) (system pos-system) dt)
  (system-do-with-components (pos) world system id
    (format t "id:~D, x:~2$, y:~2$, z:~2$~%"
            id
            (pos-x pos)
            (pos-y pos)
            (pos-z pos))))

(defun example ()
  (let* ((n 2)
         ;; make a world
         (w (make-instance 'qua:world))
         ;; make an array of 2 entities
         (e (make-array n
                        :initial-contents
                        (loop for i from 0 below n collect (make-entity w))))
         ;; make an array containing the inital pos for all entities
         (pos (make-array n
                          :initial-contents
                          (loop for i from 0 below n
                            collect (make-pos :x 1.0 :y 1.0 :z 1.0))))
         (pos-sys (make-instance 'pos-system)))

    (add-systems w pos-sys)

    (loop for i from 0 below n do
      (add-components w (aref e i)
                      (aref pos i)))

    (loop for i from 0 to 10 do
      (update-world w 0.1))))
