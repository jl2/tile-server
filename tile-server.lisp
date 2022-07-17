;; tile-server.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :tile-server)

(declaim (inline set-pixel-png increment-pixel))

(defun set-pixel-png (img x y r g b a)
  "Set a pixel in im at location x,y to color (r,g,b)"
  (setf (aref img x y 0) r)
  (setf (aref img x y 1) g)
  (setf (aref img x y 2) b)
  (setf (aref img x y 3) a))

(defun increment-pixel (img x y r g b a)
  "Increment each color component of the pixel in img at location x,y by 1"
  (let ((max-val (if (= (png:image-bit-depth img) 8) 255 65535)))
    (setf (aref img y x 0) (max 0
                                (min max-val (+ (aref img y x 0) r))))
    (setf (aref img y x 1) (max 0
                                (min max-val (+ (aref img y x 1) g))))
    (setf (aref img y x 2) (max 0
                                (min max-val (+ (aref img y x 2) b))))
    (setf (aref img y x 3) (max 0
                                (min max-val (+ (aref img y x 3) a))))))


(hunchentoot:define-easy-handler (tile-request :uri "/tile") (x y z)
  (setf (hunchentoot:content-type*) "image/png")
  (hunchentoot:log-message* :info "Tile ~a ~a ~a" x y z)
  (hunchentoot:log-message* :info "~a" (hunchentoot:headers-in*))
  (let ((stream (hunchentoot:send-headers))
        (font-file-name "/usr/local/share/fonts/JuliaMono-Regular.ttf")
        (rpng (png:make-image 256 256 4 8)))
    (bl:with-memory-image-context*
        (img ctx :width 256 :height 256)
      ((data bl:image-data)
       (font bl:font-core)
       (face bl:font-face-core)
       (point bl:point-i))

      (bl:context-set-fill-style-rgba32 ctx #16r00ffff00)
      (bl:context-fill-all ctx)

      (bl:lookup-error (bl:font-face-init face))
      (bl:lookup-error (bl:font-face-create-from-file face font-file-name 0))
      (bl:lookup-error (bl:font-init font))
      (bl:lookup-error (bl:font-create-from-face font face 22.0f0))

      (bl:lookup-error (bl:context-set-fill-style-rgba32 ctx #16rff000000))

      (setf (bl:point-i.x point) 6)
      (setf (bl:point-i.y point) 80)

      (let ((string (format nil "~a ~a ~a" x y z)))
        (cffi:with-foreign-string (str string)
          (bl:lookup-error (bl:context-fill-text-i  ctx point font str (length string) bl:+text-encoding-utf8+))))
      (bl:image-get-data img data)
      (hunchentoot:log-message* :info "~a" data)
      (let ((idata (bl:image-data.pixel-data data)))
        (hunchentoot:log-message* :info "~a" idata)
        (loop for i below 256 do
          (loop for j below 256
                for offset = (* 4 (+ i (* 256 j)))
                do
                   (let ((r (cffi:mem-aref idata :uint8 (+ 0 offset)))
                         (g (cffi:mem-aref idata :uint8 (+ 1 offset)))
                         (b (cffi:mem-aref idata :uint8 (+ 2 offset)))
                         (a (cffi:mem-aref idata :uint8 (+ 3 offset))))
                     (increment-pixel rpng i j r g b a))))))
    (png:encode rpng stream)))


(defun start-server ()
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :address "127.0.0.1" :port 8081))
        )
    (hunchentoot:start acceptor)
    acceptor))
(defun stop-server (acceptor)
  (hunchentoot:stop acceptor))
