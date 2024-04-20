(in-package #:com.thejach.ld55.sprites)

(defvar *player* nil)
(defvar *background* nil)

(defun normalize (cumber)
  (/ cumber (abs cumber)))

(defclass player (sprite cleaned-on-kill-mixin)
  ((max-v :accessor /max-v :allocation :class :initform 10)
   (shot-delay-frames :accessor /shot-delay-frames :allocation :class :initform 10)

   (ball-group :accessor .ball-group :initarg :ball-group)
   (velocity :accessor .velocity :initform #(0.0 0.0))
   (shot-delay :accessor .shot-delay :initform 0)))

(defmethod initialize-instance :after ((self player) &key)
  (setf *player* self
        (.image self) (get-texture "player.png")
        (.rect self) (get-texture-rect (.image self))
        (rect-coord (.rect self) :center) (rect-coord lgame:*screen-rect* :center)))

(defmethod update ((self player))
  (when (sdl2:mouse-state-p 1)
    (if (zerop (.shot-delay self))
        (let* ((mouse-pos (lgame.mouse:get-mouse-pos))
               (ball (make-instance 'attack-ball
                                    :starting-pos (rect-coord (.rect self) :midtop)
                                    :direction (complex (- (first mouse-pos) (rect-coord (.rect self) :centerx))
                                                        (- (second mouse-pos) (rect-coord (.rect self) :top))))))
          (lgame.sprite:add-groups ball (first (lgame.sprite:.groups self)) (.ball-group self))
          (setf (.shot-delay self) (/shot-delay-frames self)))
        ; else
        (decf (.shot-delay self))))
  (let ((move #c(0 0)))
    (when (key-pressed? :key lgame::+sdl-scancode-w+)
      (incf move #c(0 -1)))
    (when (key-pressed? :key lgame::+sdl-scancode-a+)
      (incf move #c(-1 0)))
    (when (key-pressed? :key lgame::+sdl-scancode-s+)
      (incf move #c(0 1)))
    (when (key-pressed? :key lgame::+sdl-scancode-d+)
      (incf move #c(1 0)))

    (unless (= move #c(0 0))
      (setf move (* (/max-v self) (normalize move)))
      (move-in-box self (realpart move) (imagpart move)))))

(defun move-in-box (player x y)
  (let ((center (rect-coord lgame:*screen-rect* :center))
        (offset 100))
    (lgame.rect:with-rect (r (- (first center) offset) (- (second center) offset)
                             (* 2 offset) (* 2 offset))
      (move-rect (.rect player) x y)
      (unless (lgame.rect:collide-point? r (rect-coord (.rect player) :center))
        (move-rect (.rect player) (- x) (- y))
        (move-bg x y)))))

(defclass attack-ball (sprite cleaned-on-kill-mixin)
  ((starting-pos :accessor .starting-pos :initarg :starting-pos)
   (direction :accessor .direction :initarg :direction)))

(defmethod initialize-instance :after ((self attack-ball) &key)
  (setf (.image self) (get-texture "attackball.png")
        (.rect self) (get-texture-rect (.image self))
        (rect-coord (.rect self) :center) (.starting-pos self)
        (.direction self) (* 10 (normalize (.direction self)))))

(defmethod update ((self attack-ball))
  (move-rect (.rect self) (realpart (.direction self)) (imagpart (.direction self)))
  (when (>= (lgame.pathfinding:euclidean (rect-coord (.rect self) :centerx) (rect-coord (.rect self) :centery)
                                         (first (.starting-pos self)) (second (.starting-pos self)))
            (lgame.pathfinding:euclidean 0 0 (rect-coord lgame:*screen-rect* :right) (rect-coord lgame:*screen-rect* :bottom)))
    (kill self)))

(defclass coyopotato (sprite cleaned-on-kill-mixin add-groups-mixin)
  ((max-v :accessor /max-v :allocation :class :initform 4)

   (velocity :accessor .velocity :initform #(0.0 0.0))))

(defmethod initialize-instance :after ((self coyopotato) &key position)
  (setf (.image self) (get-texture "coyopotato.png")
        (.rect self) (get-texture-rect (.image self))
        (rect-coord (.rect self) :center) position))

(defmethod update ((self coyopotato))
  (let* ((player-pos (complex (rect-coord (.rect *player*) :centerx)
                              (rect-coord (.rect *player*) :centery)))
         (our-pos (complex (rect-coord (.rect self) :centerx)
                           (rect-coord (.rect self) :centery)))
         (move (- player-pos our-pos)))
    (unless (<= (abs move) 10)
      (let ((move-amount (* 4 (normalize move))))
        (move-rect (.rect self) (realpart move-amount) (imagpart move-amount))))))

(defclass fps-display (sprite cleaned-on-kill-mixin)
  ((frame-duration :accessor .frame-duration :initform 0)))

(defmethod initialize-instance :after ((self fps-display) &key)
  (setf (.image self) nil)
  (update-fps self 1)
  (setf (.rect self) (get-texture-rect (.image self)))
  (move-rect (.rect self) 10 (- (rect-coord lgame:*screen-rect* :bottom) 50)))

(defun update-fps (self frame-duration)
  (when (/= frame-duration (.frame-duration self))
    (setf (.frame-duration self) frame-duration)
    (let* ((font (lgame.font:load-font (lgame.font:get-default-mono-font) 32))
           (msg (format nil "Frame: ~3,2$" frame-duration)) ; FPS: (/ 1000.0 frame-duration)
           (new-texture (lgame.font:render-text font msg 255 255 255)))
      (when (.image self)
        (sdl2:destroy-texture (.image self)))
      (setf (.image self) new-texture))))

(defclass score (sprite cleaned-on-kill-mixin)
  ((kills :accessor .kills :initform -1)))

(defmethod initialize-instance :after ((self score) &key)
  (setf (.image self) nil)
  (inc-score self)
  (setf (.rect self) (get-texture-rect (.image self)))
  (move-rect (.rect self) 10 10))

(defun inc-score (self)
  (incf (.kills self))
  (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 32))
         (msg (format nil "Kills: ~a" (.kills self)))
         (new-texture (lgame.font:render-text font msg 255 255 255)))
    (when (.image self)
      (sdl2:destroy-texture (.image self)))
    (setf (.image self) new-texture)))

(defclass background (sprite cleaned-on-kill-mixin)
  ((offset-x :accessor .offset-x :initform 0)
   (offset-y :accessor .offset-y :initform 0)))

(defmethod initialize-instance :after ((self background) &key)
  ; lazy way to make infinite bg, repeat bg tile in a 3x3 grid
  (let* ((tile (get-texture "desert-bg-tiled.png"))
         (screen-width (rect-coord lgame:*screen-rect* :right))
         (screen-height (rect-coord lgame:*screen-rect* :bottom))
         (full-bg (sdl2:create-texture lgame:*renderer* (lgame.display:window-pixel-format) lgame::+sdl-textureaccess-target+ (* 3 screen-width) (* 3 screen-height))))
    (lgame.render:with-render-target full-bg
      (lgame.rect:with-rect (r 0 0 screen-width screen-height)
        (loop for y below 3 do
              (loop for x below 3 do
                    (sdl2:render-copy lgame:*renderer* tile :source-rect lgame:*screen-rect* :dest-rect r)
                    (incf (rect-coord r :left) screen-width))
              (setf (rect-coord r :left) 0)
              (incf (rect-coord r :top) screen-height))))
    (setf (.image self) full-bg
          (.rect self) (get-texture-rect (.image self))
          (.offset-x self) (- screen-width)
          (.offset-y self) (- screen-height)
          *background* self)))

(defmethod kill ((self background))
  (call-next-method)
  (sdl2:destroy-texture (.image self)))

(defun move-bg (x y)
  ; not exactly accurate but...
  (lgame.sprite:do-sprite (potato com.thejach.ld55::*coyopotato-group*)
    (move-rect (.rect potato) (- x) (- y)))
  (move-rect (.rect *background*) (- x) (- y)))

(defmethod update ((self background))
  (when (>= (rect-coord (.rect self) :left) 0)
    (set-rect (.rect self) :x (.offset-x self)))
  (when (<= (rect-coord (.rect self) :left) (* 2 (.offset-x self)))
    (set-rect (.rect self) :x (.offset-x self)))
  (when (>= (rect-coord (.rect self) :top) 0)
    (set-rect (.rect self) :y (.offset-y self)))
  (when (<= (rect-coord (.rect self) :top) (* 2 (.offset-y self)))
    (set-rect (.rect self) :y (.offset-y self)))

  )

;(lgame.rect:rect-string (.rect *background*))
