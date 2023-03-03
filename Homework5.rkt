#lang racket
;; Advanced Graphics

;;----------------- Some preliminary definitions -----------------
(require (lib "graphics.ss" "graphics"))

(define graphics-window #f)

;; The dimension variable determines the size of the graphics window.
(define dimension 500)

;; Opens a new graphics window for drawing.
(define (start-graphics)
  (open-graphics)
  (set! graphics-window (open-viewport "practice" dimension dimension))
  'done)

;; Clears the current graphics window.
(define (clear)
  ((clear-viewport graphics-window)))

;; Closes the current graphics window.
(define (end-graphics)
  (close-viewport graphics-window)
  'done)

;; round-to-1000ths rounds real numbers to the nearest 1000th.
;; draw-segment uses it to improve the appearance of figures.
(define (round-to-1000ths n)
  (/ (truncate (round (* n 1000))) 1000))

(define (accumulate combine base seq)
  (if (null? seq)
      base
      (combine (car seq) (accumulate combine base (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (square x) (* x x))

;;--------------------------- Points ---------------------------
;;
;; Here is the representation for points that we will use.
;; A point is represented as a list containing the point's
;; x-coordinate and y-coordinate values.

(define (make-point x y) (list x y))
(define (x-coord point) (car point))
(define (y-coord point) (cadr point))

;;-------------------------- Segments ---------------------------
;;
;; Here is our representation for segments.  A segment is represented
;; as a list containing the segment's start and end points.

(define (make-segment point1 point2) (list point1 point2))
(define (start segment) (car segment))
(define (end segment) (cadr segment))

;; In DrScheme, the point (0,0) is positioned in the upper left
;; corner of the graphics window.  We would prefer to have (0,0)
;; be at the center of the window.  The procedure draw-segment
;; does this conversion and draws the line.
(define (draw-segment segment)
  (let ((x1 (round-to-1000ths (+ (/ dimension 2) (x-coord (start segment)))))
        (y1 (round-to-1000ths (- (/ dimension 2) (y-coord (start segment)))))
        (x2 (round-to-1000ths (+ (/ dimension 2) (x-coord (end segment)))))
        (y2 (round-to-1000ths (- (/ dimension 2) (y-coord (end segment))))))
    ((draw-line graphics-window) (make-posn x1 y1) (make-posn x2 y2))
    'done))

;;---------------------------- Figures --------------------------
;;
;; We shall represent a figure as a list of segments.
;; draw-figure takes a figure and draws it on the screen.

(define (draw-figure figure)
  (for-each draw-segment figure))

;; combine-figures takes any number of figures and combines
;; them all into a single figure.  Since figures are lists of
;; segments, combine-figures just appends the lists of segments
;; together into a single list of segments.
(define combine-figures 
  (lambda figures
    (apply append figures)))

;; make-polyline takes a list of points and returns a figure
;; consisting of segments connecting the first point to the
;; second point, the second point to the third point, and
;; so on.  The list is assumed to contain at least two points.
(define (make-polyline points)
  (cond
    ((or (null? points) (null? (cdr points))) '())
    (else
     (let ((segment (make-segment (car points) (cadr points))))
       (cons segment (make-polyline (cdr points)))))))

;; make-polygon takes a list of vertex points and returns a
;; figure in which the points are connected together by segments
;; in a closed circuit. 
(define (make-polygon vertices)
  (make-polyline (append vertices (list (car vertices)))))

;;------------------- Some examples of figures -------------------

;; Three equivalent ways of defining an "arrowhead" figure:
(define arrowhead3
  (list (make-segment '(0 0) '(50 100))
        (make-segment '(50 100) '(100 0))
        (make-segment '(100 0) '(50 30))
        (make-segment '(50 30) '(0 0))))

(define arrowhead2
  (make-polyline
   (list '(0 0) '(50 100) '(100 0) '(50 30) '(0 0))))

(define arrowhead
  (make-polygon
   (list '(0 0) '(50 100) '(100 0) '(50 30))))

;; Three equivalent ways of defining a box with vertices
;; (0 0), (0 100), (100 100), and (100 0):

(define box3
  (list (make-segment '(0 0) '(0 100))
        (make-segment '(0 100) '(100 100))
        (make-segment '(100 100) '(100 0))
        (make-segment '(100 0) '(0 0))))

(define box2
  (make-polyline
   (list '(0 0) '(0 100) '(100 100) '(100 0) '(0 0))))

(define box1
  (make-polygon
   (list '(0 0) '(0 100) '(100 100) '(100 0))))

;; This figure is the arrowhead together with the box
(define arrowhead-and-box
  (combine-figures arrowhead box1))

;; dividing-point takes a segment and a number between 0 and 1
;; (called ratio), and returns a point located on the segment
;; at some intermediate distance between its endpoints.  For
;; example, if ratio is 0, then the starting point is returned;
;; if ratio is 1, then the ending point is returned; if ratio
;; is 0.5, then the segment's midpoint is returned; if ratio is
;; 0.25, then the point 1/4 of the way along the segment is
;; returned; if ratio is 0.67, then the point 2/3 of the way
;; along the segment is returned; and so on.
(define (dividing-point segment ratio)
  (let ((x1 (x-coord (start segment)))
        (y1 (y-coord (start segment)))
        (x2 (x-coord (end segment)))
        (y2 (y-coord (end segment))))
    (make-point
     (+ x1 (* ratio (- x2 x1)))
     (+ y1 (* ratio (- y2 y1))))))

;;****************************************************************
;; PART 1: Define the procedures make-inscribed-polygon and
;; make-nested-polygons.
;;
;; make-inscribed-polygon takes a polygon and a ratio (a number
;; between 0 and 1) and returns a new polygon which fits inside
;; the original polygon.  The new polygon's vertices lie on the
;; segments of the original polygon, but are shifted away from
;; the original polygon's vertices by an amount proportional to
;; ratio.  For example:
;;
 (define box
   (make-polygon (list '(0 0) '(100 0) '(100 100) '(0 100))))
 
 (define (make-inscribed-polygon polygon ratio)
   (make-polygon
    (map (lambda (seg)
           (dividing-point seg ratio))
         polygon)))
 
 (define diamond
   (make-inscribed-polygon box 0.5))
;;
;; should define a diamond-shaped figure called "diamond", with
;; vertices (50 0), (100 50), (50 100), and (0 50).  If we then
;;
;; (draw-figure box)
;; (draw-figure diamond)
;;
;; diamond will appear inside box as a smaller square
;; rotated at a 45 degree angle.
;;
;; make-nested-polygons takes a polygon and an integer n > 0 and
;; recursively nests n-1 copies of the polygon within itself,
;; each copy being inscribed within the other.  If n = 1,
;; then the figure returned is just the original polygon.  The
;; ratio used to inscribe a polygon should be the reciprocal of
;; the number of polygons left to inscribe.  That is, the ratio
;; should be 1/n.  make-nested-polygons returns a figure consisting
;; of the original polygon and all of the nested polygons inside it.
;; For example:
;;
 (define (make-nested-polygons polygon n)
   (cond ((> n 1)
          (let ((next-poly (make-inscribed-polygon polygon (/ 1 n))))
            (append 
             polygon
             (make-nested-polygons next-poly (- n 1)))))
         (#t polygon))
   )
 
; (define box
;   (make-polygon (list '(-200 -200) '(200 -200) '(200 200) '(-200 200))))
 
;; (draw-figure (make-nested-polygons box 1)) just draws the
;; original square.
;;
;; (draw-figure (make-nested-polygons box 4)) draws the original
;; box with three smaller boxes nested inside it.
;;****************************************************************

;;(define (make-inscribed-polygon polygon ratio)
;; *** you complete this ***

;;(define (make-nested-polygons polygon n)
;; *** you complete this ***

;; make-regular-polygon returns a regular n-sided polygon of
;; radius r centered on the center point.  The radius of a
;; polygon is the distance from a vertex to its center.  You
;; don't need to understand how make-regular-polygon works.
;; You can just use it to create polygons to play around with.

(define pi 3.14159265359)

(define (make-regular-polygon n center r)
  (let* ((angle (/ (* 2 pi) n))
         (angle0 (if (even? n) (/ (+ pi angle) 2) (/ pi 2)))
         (vertices
          (map (lambda (i)
                 (let ((theta (- angle0 (* i angle))))
                   (make-point
                    (+ (x-coord center) (* r (cos theta)))
                    (+ (y-coord center) (* r (sin theta))))))
               (enumerate-interval 0 (- n 1)))))
        (make-polygon vertices)))
;; We can approximate a circle by a regular polygon with a large
;; number of sides (say 100):

(define (make-circle center radius)
  (make-regular-polygon 100 center radius))

(define (make-hex center radius)
  (make-regular-polygon 6 center radius))

;; draw-shifted-figure takes a figure and two offset values, dx and
;; dy, and draws the figure shifted by an amount dx in the horizontal
;; direction and dy in the vertical direction.  It creates a coordinate
;; mapping called coord-map that shifts the positions of points in the
;; plane.  It then calls transform-figure which applies the mapping to
;; the figure, returning a new shifted figure.
(define (draw-shifted-figure figure dx dy)
  (let ((coord-map (offset-coord-map dx dy)))
    (draw-figure (transform-figure coord-map figure))))

;; offset-coord-map takes two offset values dx and dy and returns a
;; FUNCTION mapping points to points.  This function takes a point and
;; shifts it by a certain amount, as specified by dx and dy.  For
;; example, if we (define m (offset-coord-map 100 20)), then m is
;; a function that maps point (0 0) to (100 20), point (50 50) to
;; (150 70), point (-100 -100) to (0 -80), and so on.
(define (offset-coord-map dx dy)
  (lambda (point)
    (make-point
     (+ (x-coord point) dx)
     (+ (y-coord point) dy))))

;;****************************************************************
;; PART 2: Define the procedure transform-figure, which takes a
;; coordinate mapping and a figure, applies the coordinate mapping
;; to the start and end points of each segment of the figure, and 
;; returns the resulting figure.  For example:
;;
(define cmap (offset-coord-map -100 -100))
;;
;; (draw-figure (transform-figure cmap box))
;;
;; draws square1 with its lower-left vertex at point (-100 -100)
;; instead of (0 0).
;;****************************************************************

(define (transform-figure coord-map figure)
    (map (lambda (seg)
           (let ((p1 (car seg))
                 (p2 (cadr seg)))
             (make-segment (coord-map p1) (coord-map p2))))
         figure)
  )

;(draw-figure (transform-figure cmap box))

;;----------------------------- Fractals -----------------------------
;;
;; A fractal is generated by repeatedly applying a particular trans-
;; formation to the individual components of a given figure.  We can
;; create a fractal from such a figure by repeatedly applying, to each
;; line segment, a procedure that fragments and bends it into two or
;; more pieces. To get started we need a way to transform segments.
;;
;; peak, spike, and square-wave are segment transformation procedures.
;; They each take a single segment and perform some transformation
;; on the segment, turning the segment into a FIGURE (that is, a list
;; of segments).  You don't need to understand how they work, but
;; you should understand what they do.  Just try them out:
;;
;; Create a segment of your choosing:
 (define myseg (make-segment (make-point 0 0) (make-point 100 100)))
;;
;; Draw the segment your segment so you can see what it looks like:
; (start-graphics)
; (draw-segment myseg)
;;
;; Try out the other transform procedures spike and square-wave to
;; see what they do.
;(define myseg '((0 0) (100 100)))

(define (peak segment)
  (let ((L (segment-length segment)))
    (transform-figure
     (axes-shift-coord-map segment)
     (make-polyline
      (list (make-point 0 0)
            (make-point (* 1/3 L) 0)
            (make-point (* 1/2 L) (/ L (* 2 (sqrt 3))))
            (make-point (* 2/3 L) 0)
            (make-point L 0))))))

 
(define (spike segment)
  (let ((L (segment-length segment)))
    (transform-figure
     (axes-shift-coord-map segment)
     (make-polyline
      (list (make-point 0 0)
            (make-point (* 19/40 L) 0)
            (make-point (* 1/2 L) (* 19/40 L))
            (make-point (* 21/40 L) 0)
            (make-point L 0))))))

(define (square-wave segment)
    (let ((L (segment-length segment)))
      (transform-figure
       (axes-shift-coord-map segment)
       (make-polyline
        (list (make-point 0 0)
              (make-point (* 1/4 L) 0)
              (make-point (* 1/4 L) (* 1/4 L))
              (make-point (* 1/2 L) (* 1/4 L))
              (make-point (* 1/2 L) (* -1/4 L))
              (make-point (* 3/4 L) (* -1/4 L))
              (make-point (* 3/4 L) 0)
              (make-point L 0))))))

(define (segment-length segment)
  (sqrt (+ (square (- (x-coord (start segment))
                      (x-coord (end segment))))
           (square (- (y-coord (start segment))
                      (y-coord (end segment)))))))

(define (axes-shift-coord-map segment)
  (let ((x1 (x-coord (start segment)))
        (y1 (y-coord (start segment)))
        (x2 (x-coord (end segment)))
        (y2 (y-coord (end segment))))
    (let ((angle
           (cond
             ((= x1 x2) (if (> y1 y2) (- (/ pi 2)) (/ pi 2)))
             ((> x1 x2) (+ pi (atan (/ (- y2 y1) (- x2 x1)))))
             (else (atan (/ (- y2 y1) (- x2 x1)))))))
      (lambda (point)
        (let ((x (x-coord point))
              (y (y-coord point)))
          (make-point
           (+ x1 (- (* x (cos angle)) (* y (sin angle))))
           (+ y1 (+ (* x (sin angle)) (* y (cos angle))))))))))

;; 
;; Now draw the transformed segment (remember that peak returns
;; a figure, so you have to use draw-figure to see the result):
;; (clear)
; (draw-figure (square-wave myseg))
;(square-wave myseg)
 
;;********************************************************************
;; PART 3: Define the procedure make-fractal.
;;
;; make-fractal takes a segment transformation procedure, a number n,
;; and a figure, and applies the transformation procedure to each
;; segment in the figure, recursively, n times.  If n = 0, then no
;; transformation is applied to the figure.  For example:
;;
;; (draw-figure
;;   (make-fractal peak 4 (make-regular-polygon 3 '(0 0) 180)))
;;
;; draws a fractal "snowflake" created by starting with an equilateral
;; triangle, performing the "peak" transformation on each of its
;; three sides, transforming each of the resulting segments,
;; transforming those segments, and so on, a total of 4 times.
;;
;; Try using the other segment transformations and see what you
;; get.  Also try a different number of transformations, or start
;; with a different initial figure.
;;********************************************************************
;(accumulate combine base seq)
(define (make-fractal segment-transformer n figure)
  (cond ((equal? n 0) figure)
        (#t
         (let ((next-fig
                (accumulate
                 append
                 '()
                 (map (lambda (seg)
                        (segment-transformer seg))
                      figure))))
           (append
            figure
            (make-fractal segment-transformer (- n 1) next-fig))
           ))))

;(start-graphics)
;(draw-figure        (make-regular-polygon 3 '(0 0) 180))
;(draw-figure
;(make-fractal spike 4 (make-regular-polygon 3 '(0 0) 180))
;)
;; reverse-orientation takes a figure and returns a new figure with
;; the starting and ending points of each segment reversed.  Reversing
;; the orientation of the initial figure used to create a fractal may
;; result in a very different fractal.  For example, try out the
;; following and compare with the above "snowflake" fractal:
;;
(define (reverse-orientation figure)
  (map (lambda (segment)
         (make-segment (end segment) (start segment)))
       figure))

;  (draw-figure
;    (make-fractal
;      spike 4 (reverse-orientation
;               (make-regular-polygon 3 '(0 0) 180))))



;;----------------------------- Frames -----------------------------
;;
;; This representation method for frames differs slightly from the
;; one in the book (pages 134-136).  Here we represent a frame as
;; three POINTS: its origin point and the two endpoints of its edges.
;;
;;      corner2
;;        +----------+
;;       /          /
;;      /          /
;;     /          /
;;    +----------+
;;  origin      corner1

(define (make-frame origin corner1 corner2)
  (list origin corner1 corner2))

(define (origin frame) (car frame))
(define (corner1 frame) (cadr frame))
(define (corner2 frame) (caddr frame))

(define (edge1-delta-x frame)
  (- (x-coord (corner1 frame)) (x-coord (origin frame))))

(define (edge2-delta-x frame)
  (- (x-coord (corner2 frame)) (x-coord (origin frame))))

(define (edge1-delta-y frame)
  (- (y-coord (corner1 frame)) (y-coord (origin frame))))

(define (edge2-delta-y frame)
    (- (y-coord (corner2 frame)) (y-coord (origin frame))))

;;---------------------------- Images ----------------------------
;;
;; In the following sections, we distinguish the terms "image" and
;; "figure".  As before, a figure is a list of segments, each of which
;; is specified using screen coordinates.
;;
;; An image is just a figure in which all segments are specified
;; relative to the unit square, using coordinates from 0 to 1.

;; Image/frame conversion functions:

;; figure->image-coord-map maps the interval [-dimension/2..dimension/2] 
;; to [0..1]:
(define (figure->image-coord-map point)
  (make-point
   (/ (+ (x-coord point) (/ dimension 2)) dimension)
   (/ (+ (y-coord point) (/ dimension 2)) dimension)))

;; image->figure-coord-map maps the interval [0..1] to 
;; [-dimension/2..dimension/2]:
(define (image->figure-coord-map point)
  (make-point
   (- (* (x-coord point) dimension) (/ dimension 2))
   (- (* (y-coord point) dimension) (/ dimension 2))))

;; *** Uncomment these once you have defined transform-figure ***
(define (image->figure image)
  (transform-figure image->figure-coord-map image))
;;
(define (figure->image figure)
  (transform-figure figure->image-coord-map figure))

(define unit-square
  '(((0 0) (0 1)) ((0 1) (1 1)) ((1 1) (1 0)) ((1 0) (0 0))))

;;--------------------------- Painters ---------------------------
;;
;; A painter is a procedure that knows how to draw a particular
;; image.  Painters deal with images, not figures.  When a painter
;; is applied to a particular frame, the painter draws its image
;; scaled to fit the frame.  image->painter takes an image and
;; returns a painter that knows how to draw the image in whatever
;; frame it is given.
(define (image->painter image)
  (lambda (frame)
    (draw-image-in-frame image frame)))

;; figure->painter is provided for convenience.  It takes a figure
;; (specified in screen coordinates) and returns a painter for the
;; figure.
(define (figure->painter figure)
  (image->painter (figure->image figure)))

;; (frame-coord-map frame) returns a coordinate mapping (i.e., a
;; FUNCTION from points to points) that maps points from the unit
;; square having coordinates between 0 and 1 to points inside the
;; given frame.
(define (frame-coord-map frame)
  (lambda (point)
    (make-point
     (+ (x-coord (origin frame))
        (* (edge1-delta-x frame) (x-coord point))
        (* (edge2-delta-x frame) (y-coord point)))
     (+ (y-coord (origin frame))
        (* (edge1-delta-y frame) (x-coord point))
        (* (edge2-delta-y frame) (y-coord point))))))

;;*****************************************************************
;; PART 4: Define the procedure draw-image-in-frame.
;;
;; draw-image-in-frame takes an image and a frame and draws the
;; image inside the frame.  It uses the frame-coord-map procedure
;; to create a coordinate mapping based on the given frame which
;; maps points in the unit square (having coordinates in the range
;; 0-1) to points inside the frame (having coordinates in the
;; range of the screen).
;;*****************************************************************

(define (draw-image-in-frame image frame)
  (draw-figure (transform-figure (frame-coord-map frame) image)))

;; TEST JUNK: *****************************
; (define box2
;    (make-polygon (list '(-245 -245) '(245 -245) '(245 245) '(-245 245))))
; (define box3
;    (make-polygon (list '(-0 100) '(0 180) '(130 180) '(130 100))))
; (start-graphics)
; (draw-figure box2)
; (draw-figure box3)
; (define fr (make-frame '(0 100) '(100 100) '(30 180)))
; (draw-image-in-frame (figure->image box2) fr)
; (draw-image-in-frame (figure->image arrowhead) fr)


;; transform-painter returns a new painter that draws the same
;; image as the original painter, but oriented in a different
;; way.  new-origin, new-corner1, and new-corner2 are points in
;; the _unit square_ which determine how the image is transformed.
(define (transform-painter painter new-origin new-corner1 new-corner2)
  (lambda (frame)
    (let ((coord-map (frame-coord-map frame)))
      (painter (make-frame (coord-map new-origin)
                           (coord-map new-corner1)
                           (coord-map new-corner2))))))

;; A few painter transformations, defined using transform-painter:
(define (flip-vert painter)
  (transform-painter painter '(0 1) '(1 1) '(0 0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter '(.5 .5) '(1 .5) '(.5 1)))

(define (rotate90 painter)
  (transform-painter painter '(1 0) '(1 1) '(0 0)))

(define (squash-inwards painter)
  (transform-painter painter '(0 0) '(.65 .35) '(.35 .65)))

(define (stretch painter)
  (transform-painter painter '(0 0) '(1.5 0) '(0 .5)))

;;*****************************************************************
;; PART 5: Define three new painter transformations: one that flips
;; the image produced by a painter horizontally, one that rotates
;; the image 180 degrees counterclockwise, and one that rotates the
;; image 270 degrees counterclockwise.
;;*****************************************************************

(define (flip-horiz painter)
  (transform-painter painter '(1 0) '(0 0) '(1 1)))

(define (rotate180 painter)
  (transform-painter painter '(1 1) '(0 1) '(1 0)))

(define (rotate270 painter)
  (transform-painter painter '(0 1) '(0 0) '(1 1)))
  
;; A few painter operations:
(define (beside painter1 painter2)
  (let ((left-painter
         (transform-painter painter1 '(0 0) '(.5 0) '(0 1)))
        (right-painter
         (transform-painter painter2 '(.5 0) '(1 0) '(.5 1))))
    (lambda (frame)
      (left-painter frame)
      (right-painter frame))))

(define (below painter1 painter2)
  (let ((bottom-painter
         (transform-painter painter1 '(0 0) '(1 0) '(0 .5)))
        (top-painter
         (transform-painter painter2 '(0 .5) '(1 .5) '(0 1))))
    (lambda (frame)
      (bottom-painter frame)
      (top-painter frame))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;;****************************************************************
;; PART 6: Define three new painter operations: up-split,
;; down-split, and left-split.  They are analogous to right-split,
;; except they recursively split the image towards the top,
;; bottom, and left side (respectively) of the frame.
;;****************************************************************

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; (define box2
;    (make-polygon (list '(-245 -245) '(245 -245) '(245 245) '(-245 245))))
; (define box3
;    (make-polygon (list '(-0 100) '(0 180) '(130 180) '(130 100))))
; (start-graphics)
; ;(draw-figure box2)
; ;(draw-figure box3)
; (define fr (make-frame '(0 100) '(100 100) '(30 180)))
; ;(draw-image-in-frame (figure->image box2) fr)
; ;(draw-image-in-frame (figure->image box) fr)
; ((figure->painter arrowhead) fr)
; ( (rotate90 (figure->painter arrowhead)) fr)
; ;((flip-vert (flip-horiz (figure->painter box3))) fr)


(define (down-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (down-split painter (- n 1))))
        (below (beside smaller smaller) painter ))))

(define (left-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (left-split painter (- n 1))))
        (beside (below smaller smaller) painter ))))


;;---------- Examples of frames, images, and painters -----------

;; *** Uncomment these to test the completed code ***
(define arrow-image
  (figure->image arrowhead))
;;
(define arrow-painter
  (image->painter arrow-image))
;;
(define frame1
  (let ((half (/ dimension 2)))
    (make-frame (list (* -1 half) (* -1 half))
                (list half (* -1 half))
                (list (* -1 half) half))))

(define frame2
  (make-frame '(0 100) '(100 100) '(30 180)))

(define frame3
  (make-frame '(-150 -50) '(125 -10) '(-80 50)))

(define border-image
  (list (make-segment '(0 0) '(0 1))
        (make-segment '(0 1) '(1 1))
        (make-segment '(1 1) '(1 0))
        (make-segment '(1 0) '(0 0))))

(define kite-image
  (list (make-segment '(.5 0) '(0 .5))
        (make-segment '(0 .5) '(.5 1))
        (make-segment '(.5 1) '(1 .5))
        (make-segment '(1 .5) '(.5 0))
        (make-segment '(0 .5) '(1 .5))
        (make-segment '(.5 0) '(.5 1))))

(define triangle-image
  '(((0 0) (1 0.5)) ((1 0.5) (0 1)) ((0 1) (0 0))))

(define triangle-painter
  (image->painter triangle-image))

;(define A-image
;  '(((0 0) (0.5 1)) ((0.5 1) (1 0)) ((0.25 0.5) (0.75 0.5))))

;(define A-painter
;  (image->painter A-image))

(define window-image
  (combine-figures
   border-image
   (make-polyline (list '(0.5 0) '(0.5 1)))
   (make-polyline (list '(0 0.5) '(1 0.5)))))

(define wave-image
  (combine-figures
   (make-polyline
    (list '(0 .8) '(.2 .6) '(.3 .7) '(.4 .7) '(.35 .85) '(.4 1)))
   (make-polyline
    (list '(.6 1) '(.65 .85) '(.6 .7) '(.7 .7) '(1 .3)))
   (make-polyline
    (list '(1 .2) '(.65 .5) '(.7 0)))
   (make-polyline
    (list '(.6 0) '(.5 .3) '(.4 0)))
   (make-polyline
    (list '(.3 0) '(.35 .5) '(.3 .55) '(.2 .4) '(0 .7)))))

(define wave (image->painter wave-image))
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (flipped-pairs wave))
(define window (image->painter window-image))
(define kite (image->painter kite-image))
(define border (image->painter border-image))

;(start-graphics)
;(border frame1)
;(kite frame1)
;(window frame1)
;(wave4 frame1)
;(wave2 frame1)

;; To recreate Figure 2.9 from the book, do the following:
(start-graphics)
((square-limit wave 4) frame1)

;;****************************************************************
;; PART 7: Create your own picture using any combination of the 
;; picture language procedures created above.
;;****************************************************************


;; Once you have make-inscribed-polygon, make-nested-polygons, and
;; make-fractal working, here are a few examples to try out
;; (some of them may take a while to draw):

;(start-graphics)
(clear)
(draw-figure
 (make-nested-polygons (make-regular-polygon 3 '(0 -30) 200) 30))

(display "nested polygons 1: ")
(display "press 'y' then 'enter' to continue: ")
(read)

(clear)
(draw-figure
 (make-nested-polygons (make-regular-polygon 6 '(0 0) 200) 43))

(display "nested polygons 2: ")
(display "press 'y' then 'enter' to continue: ")
(read)

(clear)
(draw-figure
 (make-fractal
  peak 1 (make-regular-polygon 3 '(0 0) 180)))

(display "fractal 1: ")
(display "press 'y' then 'enter' to continue: ")
(read)

(clear)
(draw-figure
 (make-fractal
  peak 2 (make-regular-polygon 3 '(0 0) 180)))

(display "fractal 2: ")
(display "press 'y' then 'enter' to continue: ")
(read)

(clear)
(draw-figure
 (make-fractal
  peak 3 (make-regular-polygon 3 '(0 0) 180)))

(display "fractal 3: ")
(display "press 'y' then 'enter' to continue: ")
(read)

(clear)
(draw-figure
 (make-fractal
  peak 4 (make-regular-polygon 3 '(0 0) 180)))
                                   
(display "fractal 4: ")
(display "press 'y' then 'enter' to continue: ")
(read)

(clear)
(draw-figure
 (make-fractal
  spike 4
  (reverse-orientation
   (make-regular-polygon 4 '(0 0) 200))))

(display "fractal 5: ")
(display "press 'y' then 'enter' to continue: ")
(read)

;; The remainder of this file gives some examples of using frames and
;; painters to draw images on the screen.  You should work through
;; these examples step by step, trying things out in Scheme as you go.
;; In order for these examples to work, you'll first need to define
;; the procedures make-polyline, make-polygon, transform-figure, and
;; draw-image-in-frame.

;; Let's first define a figure that looks like a capital "A" surrounded
;; by a square border:

(define A-figure
  (combine-figures
   (make-polyline (list '(-200 -200) '(0 200) '(200 -200)))
   (make-polyline (list '(-120 -40) '(120 -40)))
   (make-polygon (list '(-200 -200) '(-200 200) '(200 200) '(200 -200)))))

;; Now let's draw it on the screen so we can see what it looks like:

(clear)
(draw-figure A-figure)

(display "A figure: ")
(display "press 'y' then 'enter' to continue: ")
(read)

;; Next, let's define an "image" of capital A -- that is, a figure that
;; uses unit-square coordinates (in the range 0 to 1) instead of normal
;; screen coordinates (in the range -200 to 200):

(define A-image
  (combine-figures
   (make-polyline (list '(0 0) '(.5 1) '(1 0)))
   (make-polyline (list '(.2 .4) '(.8 .4)))
   (make-polygon (list '(0 0) '(0 1) '(1 1) '(1 0)))))

;; What happens when we try to draw the image using "draw-figure"?
;; Try it out:

(clear)
(draw-figure A-image)

(display "A image: ")
(display "press 'y' then 'enter' to continue: ")
(read)

;; draw-figure expects screen coordinates, so we can't use draw-figure
;; directly to draw images.  To draw an image, we first create a
;; "painter" for it:

(define A-painter (image->painter A-image))

;; A "painter" is a procedure that waits for a frame.  As soon as
;; it is called on a frame, it transforms its image from unit-square
;; coordinates to screen coordinates, using the coordinate mapping of
;; the frame it is given.  For example, let's create a few frames and
;; call A-painter with them.  A-painter draws its image of a capital
;; "A" in whatever frame it is given:

;; f0 is a frame encompassing the entire screen:
(define f0 (make-frame '(-200 -200) '(200 -200) '(-200 200)))

(clear)
(A-painter f0)

;; f1 is a rotated, diamond-shaped frame in the lower right portion of
;; the screen:
(define f1 (make-frame '(0 -150) '(100 -100) '(50 -30)))

(A-painter f1)

;; f2 is a rotated, flattened-out rectangular frame in the upper left
;; part of the screen:
(define f2 (make-frame '(-150 150) '(0 0) '(-100 200)))

(A-painter f2)

;; f3 is a tall, skinny frame on the bottom left side of the screen:
(define f3 (make-frame '(-180 -200) '(-100 -200) '(-180 50)))

(A-painter f3)

;; f4 is a small (but undistorted) frame in the upper right corner of
;; the screen:
(define f4 (make-frame '(150 150) '(200 150) '(150 200)))

(A-painter f4)

(display "A painter in many frames: ")
(display "press 'y' then 'enter' to continue: ")
(read)

;; We have several operations on painters available.  One of them,
;; "flip-vert", takes a painter and gives back a new painter that
;; draws the original painter's image flipped vertically within its
;; frame.  Let's create a painter that will draw the capital "A"
;; upside-down in whatever frame we give it:

(define inverted-A-painter
  (flip-vert A-painter))

(clear)
(inverted-A-painter f1)
(inverted-A-painter f2)
(inverted-A-painter f3)
(inverted-A-painter f4)

(display "inverted A painter in many frames: ")
(display "press 'y' then 'enter' to continue: ")
(read)

;; shrink-to-upper-right is another painter operation.  It creates a
;; new painter that shrinks the original painter's image into the
;; upper right quarter of the frame.  Let's try drawing the original
;; capital "A" together with a shrunk version of it, so you can see
;; the difference:

(define shrunk-A-painter
  (shrink-to-upper-right A-painter))

(clear)
(A-painter f1)
(shrunk-A-painter f1)

(A-painter f2)
(shrunk-A-painter f2)

(A-painter f3)
(shrunk-A-painter f3)

(A-painter f4)
(shrunk-A-painter f4)

(display "shrunk A painter and original A painter in many frames: ")
(display "press 'y' then 'enter' to continue: ")
(read)

;; The nice thing about painter operations is that we can combine
;; them together very easily.  Let's create a new painter that
;; both flips its image and shrinks it to the upper right quarter
;; of whatever frame it is given:

(define shrunk-inverted-A-painter
  (shrink-to-upper-right (flip-vert A-painter)))

;; First let's see the result on the full screen:

(clear)
(shrunk-inverted-A-painter f0)

(display "shrunk-inverted A painter: ")
(display "press 'y' then 'enter' to continue: ")
(read)

;; Let's try the new painter out using the other frames.  We'll also
;; draw the original painter's image for comparison:

(clear)
(A-painter f1)
(shrunk-inverted-A-painter f1)

(A-painter f2)
(shrunk-inverted-A-painter f2)

(A-painter f3)
(shrunk-inverted-A-painter f3)

(A-painter f4)
(shrunk-inverted-A-painter f4)

(display "shrunk-inverted A painter and original A painter in many frames: ")
(display "press 'y' then 'enter' to continue: ")
(read)

;; The "beside" and "below" painter operations are useful for
;; combining two painters into a single painter that paints the
;; original painters' images beside each other, or one below the
;; other.  Let's create a painter that paints two capital A's
;; next to each other within a single frame:

(define AA-painter
  (beside A-painter A-painter))

;; First we'll draw the new painter's image in the full-screen
;; so we can see it clearly:

(clear)
(AA-painter f0)

(display "beside A painter: ")
(display "press 'y' then 'enter' to continue: ")
(read)

;; Since AA-painter is a painter, we can call it on whatever frame
;; we like.  As usual, it draws its image scaled to fit the frame:

(clear)
(AA-painter f1)

(display "beside A painter in frame f1: ")
(display "press 'y' then 'enter' to continue: ")
(read)


;; Now let's combine A-painter and AA-painter into a new painter
;; that draws a single capital "A" below a pair of capital "A"s:

(define AAA-painter
  (below A-painter AA-painter))

(clear)
(AAA-painter f0)

(display "beside and below A painter: ")
(display "press 'y' then 'enter' to continue: ")
(read)


(clear)
(AAA-painter f1)

(display "beside and below A painter in frame f1: ")
(display "press 'y' then 'enter' to continue: ")
(read)


;; Let's see what the "flipped-pairs" operation does:

(define flip-A-painter (flipped-pairs A-painter))

(clear)
(flip-A-painter f0)

(display "flipped pairs A painter: ")
(display "press 'y' then 'enter' to continue: ")
(read)


(clear)
(flip-A-painter f1)

(display "flipped pairs A painter in frame f1: ")
(display "press 'y' then 'enter' to continue: ")
(read)


;; We can even combine painters recursively to create some dramatic
;; effects.  The "right-split" operation takes a painter and returns
;; a new painter that "splits" the original painter's image recursively
;; toward the right side of the frame.  As with fractals, we have to
;; specify some limit to the recursion.  Let's define a painter that
;; splits the capital "A" image recursively to the right 4 times:

(define right-split-A-painter
  (right-split A-painter 4))

(clear)
(right-split-A-painter f0)

(display "right split A painter: ")
(display "press 'y' then 'enter' to continue: ")
(read)


;; right-split-A-painter is a painter, so we can draw its image in
;; whatever frame we like:

(clear)
(right-split-A-painter f1)
(right-split-A-painter f2)
(right-split-A-painter f3)
(right-split-A-painter f4)

(display "right split A painter in many frames: ")
(display "press 'y' then 'enter' to continue: ")
(read)


;; Let's try right-splitting the flip-A-painter:

(define flipped-and-split-A
  (right-split flip-A-painter 4))

(clear)
(flipped-and-split-A f0)

(display "right split and flipped A painter: ")
(display "press 'y' then 'enter' to continue: ")
(read)


;; Finally, let's see what "corner-split" and "square-limit" do
;; (these may take some time to draw):

(define wow1
  (corner-split A-painter 4))

(clear)
(wow1 f0)

(display "wow1: ")
(display "press 'y' then 'enter' to continue: ")
(read)


(define wow2
  (square-limit A-painter 3))

(clear)
(wow2 f0)

(display "wow2: ")
(display "press 'y' then 'enter' to continue: ")
(read)

(clear)
(wow2 f1)

(display "wow2 in frame f1: ")
(display "press 'y' then 'enter' to continue: ")
(read)

(define triangle
  (make-nested-polygons
    (make-regular-polygon 3 '(0 -30) 200) 10))

(define wow3
  (square-limit (figure->painter triangle) 3))

(clear)
(wow3 f0)

(display "wow3: ")
(display "press 'y' then 'enter' to continue: ")
(read)


(clear)
(wow3 f1)

(display "wow3 in frame f1: ")
(display "press 'y' then 'enter' to continue: ")
(read)