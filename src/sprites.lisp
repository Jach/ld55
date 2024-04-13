(defpackage #:com.thejach.ld55.sprites
  (:use #:cl)
  (:export #:player #:coyopotato #:attack-ball
           #:fps-display #:update-fps

           #:update #:draw)

  (:import-from #:lgame.loader
                #:get-texture)
  (:import-from #:lgame.event
                #:key-pressed?)
  (:import-from #:lgame.sprite
                #:sprite
                #:cleaned-on-kill-mixin
                #:add-groups-mixin
                #:.image
                #:.rect
                #:.flip

                #:update
                #:draw
                #:kill)
  (:import-from #:lgame.rect
                #:rect-coord
                #:move-rect
                #:get-texture-rect))

(in-package #:com.thejach.ld55.sprites)

(defvar *player* nil)

(defun normalize (cumber)
  (/ cumber (abs cumber)))

(defclass player (sprite cleaned-on-kill-mixin)
  ((max-v :accessor /max-v :allocation :class :initform 10)
   (shot-delay-frames :accessor /shot-delay-frames :allocation :class :initform 10)

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
          (lgame.sprite:add-groups ball (first (lgame.sprite:.groups self)))
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
      (move-rect (.rect self) (realpart move) (imagpart move)))))

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
  (move-rect (.rect self) 10 (- (rect-coord lgame:*screen-rect* :bottom) 30)))

(defun update-fps (self frame-duration)
  (when (/= frame-duration (.frame-duration self))
    (setf (.frame-duration self) frame-duration)
    (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 20))
           (msg (format nil "FPS: ~a" frame-duration))
           (new-texture (lgame.font:render-text font msg 255 255 255)))
      (when (.image self)
        (sdl2:destroy-texture (.image self)))
      (setf (.image self) new-texture))))
