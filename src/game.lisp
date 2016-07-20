(in-package :botclean)

(defun dp-play (field-str &key (visualize t) (max-moves 100))
  (let ((root-state (parse-field field-str)))
    (labels ((%step (state num-moves)
               (when visualize
                 (visualize-state state *standard-output*))
               (if (or (= num-moves 0)
                       (= (length (state-dirty-cells state)) 0))
                   state
                   (let* ((move (select-next-move state))
                          (new-state (next-state state move)))
                     (%step new-state (1- num-moves))))))
      (%step root-state max-moves))))

(defun select-next-move (state)
  (declare (ignorable state))
  nil)

(defun next-state (state move)
  (declare (ignorable move))
  state)

(defun visualize-state (state stream)
  (format stream "Moves: ~A~%Dirty cells: ~A~%"
          (state-moves-count state)
          (length (state-dirty-cells state)))
  (loop for y from 0 to (1- (field-height (state-field state))) do
       (loop for x from 0 to (1- (field-width (state-field state))) do
            (let* ((pos (make-pos :x x :y y ))
                   (val (get-cell (state-field state) pos)))
              (princ (if (equalp pos (state-bot-pos state))
                         #\b
                         (case val
                           (:dirt #\d)
                           (:wall #\#)
                           (otherwise #\-)))
                     stream)))
       (terpri stream)))
