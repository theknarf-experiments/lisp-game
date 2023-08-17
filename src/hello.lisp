#|Public domain. Does not unwind-protect errors like it should.|#
(in-package #:cl-user)
(defpackage #:sdl-intro
  (:use #:common-lisp))
(in-package #:sdl-intro)
 
(ql:quickload "sdl2")
 
(defun null-ptr? (alien-val)
  (cffi:null-pointer-p (autowrap:ptr alien-val)))
 
(defun init-window (w h &optional screen renderer)
  
  ;(unless (zerop (sdl2-ffi.functions:sdl-init sdl2-ffi:+sdl-init-video+))
 ;  (error "Could not init"))
   (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+))

  (setf screen (sdl2-ffi.functions:sdl-create-window
                 "Tiny Renderer"
                 sdl2-ffi:+sdl-windowpos-undefined+ sdl2-ffi:+sdl-windowpos-undefined+ ; let the OS position window
                 w h
                 sdl2-ffi:+sdl-window-opengl+))
  (if (null-ptr? screen)
      (error "Could not make window screen"))
 
  (setf renderer (sdl2-ffi.functions:sdl-create-renderer
                   screen -1 0)) ; default monitor, no flags like vsync
  (if (null-ptr? renderer)
      (error "Could not make renderer"))
  (values renderer screen)
)

(defun render-buffer (buffer texture renderer sz w &optional buffer-ptr)
  (setf buffer-ptr (cffi:foreign-array-alloc buffer `(:array :uint32 ,sz)))
  ;(cffi:lisp-array-to-foreign buffer buffer-ptr `(:array :uint32 ,sz)) ; if ptr allocated separately
 
  (unless (zerop (sdl2-ffi.functions:sdl-update-texture texture nil buffer-ptr (* w (cffi:foreign-type-size :uint32))))
    (error "Could not update texture"))
 
  (cffi:foreign-array-free buffer-ptr)
 
  ; technically clearing is not needed since the texture is the size of the screen, but
  ; if another program drew over the display (like steam overlay) this would make sure
  ; it doesn't hang around.
  (sdl2-ffi.functions:sdl-set-render-draw-color renderer 0 0 0 255)
  (sdl2-ffi.functions:sdl-render-clear renderer)
  (sdl2-ffi.functions:sdl-render-copy renderer texture nil nil) ; copy the whole thing
  (sdl2-ffi.functions:sdl-render-present renderer)
)

(defun render-then-quit (&aux (w 400) (h 300) sz screen renderer texture buffer event frame)
  (setf sz (* w h))

  (setf (values renderer screen) (init-window w h))

  (setf texture (sdl2-ffi.functions:sdl-create-texture
                  renderer
                  sdl2-ffi:+sdl-pixelformat-argb8888+
                  sdl2-ffi:+sdl-textureaccess-streaming+
                  w h))
  (if (null-ptr? texture)
      (error "Could not make texture"))
 
  
 


  (format t "Beginning main loop.~%")
  (setf frame 0)
   (loop while (<= frame 120) do
    ;(format t "Frame: ~D~%" frame)
    (setf frame (+ 1 frame))
      (setf buffer (make-array sz :initial-element 0 :element-type '(unsigned-byte 32)))
      (loop for x from (* w (+ 50 frame)) to (* w (+ 150 frame)) do
            (setf (aref buffer x) #xFFAA0033)) ; red

      (render-buffer buffer texture renderer sz w)
    )
 
  (sdl2-ffi.functions:sdl-destroy-texture texture)
  (sdl2-ffi.functions:sdl-destroy-renderer renderer)
  (sdl2-ffi.functions:sdl-destroy-window screen)
 
  (sdl2-ffi.functions:sdl-quit)
)
 
(render-then-quit)