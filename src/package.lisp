(defpackage #:com.thejach.ld55.sprites
  (:use #:cl)
  (:export #:player #:coyopotato #:attack-ball
           #:fps-display #:update-fps
           #:background
           #:score #:inc-score

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
                #:set-rect
                #:get-texture-rect))

(defpackage #:com.thejach.ld55
  (:use #:cl #:com.thejach.ld55.sprites)
  (:export #:main)
  (:import-from #:lgame.event
                #:do-event
                #:event-type
                #:key-scancode)
  )

