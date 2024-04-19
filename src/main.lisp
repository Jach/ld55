(defpackage #:com.thejach.ld55
  (:use #:cl #:com.thejach.ld55.sprites)
  (:export #:main)
  (:import-from #:lgame.event
                #:do-event
                #:event-type
                #:key-scancode)
  )
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
(defvar *all-group* nil)
(defvar *coyopotato-group* nil)

(defun spawn-coyopotato ()
  (make-instance 'coyopotato :position #(0 0) :groups (list *coyopotato-group* *all-group*)))

(defun main ()
  (lgame:init)
  (lgame.loader:create-texture-loader (assets-dir))
  (lgame.display:create-centered-window "I tried to summon Coyo but I kept getting hostile Coyopotatoes" (first +window-size+) (second +window-size+))
  (lgame.display:create-renderer)
  (lgame.display:set-logical-size (first +logical-size+) (second +logical-size+))
  ;(sdl2:hide-cursor)

  (sdl2:pump-events)
  (lgame::sdl-raise-window lgame:*screen*)

  (setf *player* (make-instance 'player)
        *fps-display* (make-instance 'fps-display)

        *coyopotato-group* (make-instance 'lgame.sprite:group)
        *all-group* (make-instance 'lgame.sprite:ordered-group))
  (lgame.sprite:add-groups *player* *all-group*)
  (lgame.sprite:add-groups *fps-display* *all-group*)

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
        (spawn-coyopotato))))

  (lgame.sprite:update *all-group*)

  (lgame.render:clear)

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
