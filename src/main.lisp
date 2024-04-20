(in-package #:com.thejach.ld55)

(defun app-root ()
  (asdf:system-source-directory "com.thejach.ld55"))

(defun assets-dir ()
  (merge-pathnames #p"assets/" (app-root)))

(defparameter +window-size+ '(1280 720))
(defparameter +logical-size+ '(1280 720))
(defvar *full-screen* nil)

(defvar *player* nil)
(defvar *fps-display* nil)
(defvar *score* nil)
(defvar *background* nil)
(defvar *all-group* nil)
(defvar *coyopotato-group* nil)
(defvar *ball-group* nil)

(defvar *spawn-spots* (list #(0 0) #(0 720) #(1280 0) #(1280 720)))
(defun spawn-coyopotato ()
  (make-instance 'coyopotato :position (alexandria:random-elt *spawn-spots*) :groups (list *coyopotato-group* *all-group*)))

(defun main ()
  (lgame:init)
  (lgame.loader:create-texture-loader (assets-dir))
  (lgame.display:create-centered-window "I tried to summon Coyo but I kept getting hostile Coyopotatoes" (first +window-size+) (second +window-size+))
  (lgame.display:create-renderer)
  (lgame.display:set-logical-size (first +logical-size+) (second +logical-size+))
  (sdl2:hide-cursor)

  (sdl2:pump-events)
  (lgame::sdl-raise-window lgame:*screen*)

  (setf *ball-group* (make-instance 'lgame.sprite:group))
  (setf *player* (make-instance 'player :ball-group *ball-group*)
        *fps-display* (make-instance 'fps-display))
  (setf *score* (make-instance 'score))
  (setf *background* (make-instance 'background))
  (setf *coyopotato-group* (make-instance 'lgame.sprite:group))
  (setf *all-group* (make-instance 'lgame.sprite:ordered-group))
  (lgame.sprite:add-groups *background* *all-group*)
  (lgame.sprite:add-groups *player* *all-group*)
  (lgame.sprite:add-groups *fps-display* *all-group*)
  (lgame.sprite:add-groups *score* *all-group*)

  (lgame.time:clock-start)
  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (livesupport:continuable
            (game-tick)))

    (lgame.sprite:do-sprite (sprite *all-group*)
      (lgame.sprite:kill sprite))
    (lgame:quit)))

(defun game-tick ()
  (lgame.event:do-event (event)
    (when (or
            (= (event-type event) lgame::+sdl-quit+)
            (and (= (event-type event) lgame::+sdl-keydown+)
                 (= (key-scancode event) lgame::+sdl-scancode-escape+)))
      (lgame.time:clock-stop))
    (when (= (event-type event) lgame::+sdl-keyup+)
      (when (= (key-scancode event) lgame::+sdl-scancode-f+)
        (if *full-screen*
            (progn (lgame::sdl-set-window-fullscreen lgame:*screen* 0) (setf *full-screen* nil))
            (progn (lgame::sdl-set-window-fullscreen lgame:*screen* lgame::+sdl-window-fullscreen-desktop+) (setf *full-screen* t))))

      (when (= (key-scancode event) lgame::+sdl-scancode-space+)
        (spawn-coyopotato))

      (when (= (key-scancode event) lgame::+sdl-scancode-t+)
        (lgame.display:screenshot-png "/tmp/ld55.png"))

      (when (= (key-scancode event) lgame::+sdl-scancode-h+)
        (if (plusp (length (lgame.sprite:.groups *fps-display*)))
            (lgame.sprite:remove-groups *fps-display* *all-group*)
            (lgame.sprite:add-groups *fps-display* *all-group*)))
      )
    )

  (lgame.sprite:update *all-group*)
  (let ((sprites-to-kill ()))
    (dolist (collisions (lgame.sprite:group-collide *coyopotato-group* *ball-group*))
      (let ((potato (first collisions))
            (balls (rest collisions)))
        (lgame.sprite:kill potato)
        (inc-score *score*)
        (setf sprites-to-kill (union sprites-to-kill balls))))
    (mapc #'lgame.sprite:kill sprites-to-kill))

  (lgame.render:clear)

  ;(lgame.render:blit (lgame.sprite:.image *background*) nil)
  ;(lgame.rect:with-rect (r 0 0 1280 720)
  ;  (lgame.render:blit (lgame.loader:get-texture "desert-bg-tiled.png") r))
  (lgame.sprite:draw *all-group*)
  (lgame.render:with-draw-color (128 255 128)
    (let ((pos (lgame.mouse:get-mouse-pos)))
      (lgame.draw:render-fill-circle lgame:*renderer* (first pos) (second pos) 15)))

  (lgame.render:present)

  (livesupport:update-repl-link)
  (multiple-value-bind (duration delay) (lgame.time:clock-tick 60)
    (update-fps *fps-display* (+ (* 1 duration) (* 1 delay)))))

(eval-when (:execute)
  (main))

