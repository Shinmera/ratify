#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.css)

(defvar *css-color-names* '(AliceBlue AntiqueWhite Aqua Aquamarine Azure Beige Bisque Black BlanchedAlmond Blue BlueViolet Brown BurlyWood CadetBlue Chartreuse Chocolate Coral CornflowerBlue Cornsilk Crimson Cyan DarkBlue DarkCyan DarkGoldenrod DarkGray DarkGreen DarkKhaki DarkMagenta DarkOliveGreen DarkOrange DarkOrchid DarkRed DarkSalmon DarkSeaGreen DarkSlateBlue DarkSlateGray DarkTurquoise DarkViolet DeepPink DeepSkyBlue DimGray DodgerBlue FireBrick FloralWhite ForestGreen Fuchsia Gainsboro GhostWhite Gold Goldenrod Gray Green GreenYellow Honeydew HotPink IndianRed Indigo Ivory Khaki Lavender LavenderBlush LawnGreen LemonChiffon LightBlue LightCoral LightCyan LightGoldenrodYellow LightGreen LightGrey LightPink LightSalmon LightSeaGreen LightSkyBlue LightSlateGray LightSteelBlue LightYellow Lime LimeGreen Linen Magenta Maroon MediumAquamarine MediumBlue MediumOrchid MediumPurple MediumSeaGreen MediumSlateBlue MediumSpringGreen MediumTurquoise MediumVioletRed MidnightBlue MintCream MistyRose Moccasin NavajoWhite Navy OldLace Olive OliveDrab Orange OrangeRed Orchid PaleGoldenrod PaleGreen PaleTurquoise PaleVioletRed PapayaWhip PeachPuff Peru Pink Plum PowderBlue Purple Red RosyBrown RoyalBlue SaddleBrown Salmon SandyBrown SeaGreen Seashell Sienna Silver SkyBlue SlateBlue SlateGray Snow SpringGreen SteelBlue Tan Teal Thistle Tomato Turquoise Violet Wheat White WhiteSmoke Yellow YellowGreen))

(defun css-argslist (argslist start end)
  (unless (and (<= 2 (- end start))
               (char= (aref argslist start) #\()
               (char= (aref argslist (1- end)) #\)))
    (ratification-error argslist "Invalid arguments list."))
  (cl-ppcre:split "\\s*,\\s*" (subseq argslist (1+ start) (1- end))))

(defun test-rgb (vals)
  (loop for val in vals
        do (unless (<= 0 (parse-integer val) 255)
             (ratification-error val "RGB values must be an integer between 0 and 255."))))

(defun test-percentage (p)
  (unless (char= (aref p (1- (length p))) #\%)
    (ratification-error p "Percentage sign missing."))
  (unless (<= 0 (parse-integer p :end (1- (length p))) 100)
    (ratification-error p "Percentage must be an integer between 0 and 100.")))

(defun test-hsl (vals)
  (destructuring-bind (h s l) vals
    (unless (<= 0 (parse-integer h) 360)
      (ratification-error h "Hue must be an integer between 0 and 360."))
    (test-percentage s)
    (test-percentage l)))

(define-test color (color start end)
  (unless (<= 4 (- end start))
    (ratification-error color "Color must be at least four characters long."))
  (cond ((string= color "#" :start1 start :end1 (1+ start))
         (unless (or (= 4 (length color))
                     (= 7 (length color)))
           (ratification-error color "A HEX colour must be either 3 or 6 ciphers."))
         (parse-integer color :radix 16 :start (1+ start) :end end))
        
        ((string= color "rgba" :start1 start :end1 (+ start 4))
         (let ((args (css-argslist color (+ start 4) end)))
           (unless (= 4 (length args))
             (ratification-error color "RGBA requires 4 arguments."))
           (test-rgb (subseq args 0 3))
           (unless (<= 0 (parse-float:parse-float (fourth args)) 1)
             (ratification-error color "Alpha value must be between 0.0 and 1.0"))))
        
        ((string= color "rgb" :start1 start :end1 (+ start 3))
         (let ((args (css-argslist color (+ start 3) end)))
           (unless (= 3 (length args))
             (ratification-error color "RGB requires 3 arguments."))
           (test-rgb args)))
        
        ((string= color "hsla" :end1 4)
         (let ((args (css-argslist color (+ start 4) end)))
           (unless (= 4 (length args))
             (ratification-error color "RGBA requires 4 arguments."))
           (test-hsl (subseq args 0 3))
           (unless (<= 0 (parse-float:parse-float (fourth args)) 1)
             (ratification-error color "Alpha value must be between 0.0 and 1.0"))))
        
        ((string= color "hsl" :end1 3)
         (let ((args (css-argslist color (+ start 3) end)))
           (unless (= 3 (length args))
             (ratification-error color "RGB requires 3 arguments."))
           (test-hsl args)))
        
        (T
         (unless (find color *css-color-names* :test #'string-equal)
           (ratification-error color "Color ~s is not a known colour name or scheme." color)))))

(define-test property (property start end)
  (loop with in-paren = 0
        with in-string = NIL
        for prev = #\Space then char
        for i from start below end
        for char = (char property i)
        do (unless (char= #\\ prev)
             (case char
               (#\( (incf in-paren))
               (#\) (decf in-paren))
               (#\" (setf in-string (not in-string)))
               (T (when (and (not in-string) (= in-paren 0) (find char "{}[];:/*\\"))
                    (ratification-error property "Character ~a is not allowed outside of strings." char)))))
        finally (when (or in-string (/= in-paren 0))
                  (ratification-error property "Property ~s contains unbalanced delimiters." property))))
