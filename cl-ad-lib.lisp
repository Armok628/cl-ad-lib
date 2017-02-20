(setf *random-state* (make-random-state t))

(defparameter *default-vocabulary*
  '(([noun] "man" "ball" "woman" "table" "thing" "box" "wall" "bar" "place" "food" "dog" "animal" "computer")
    ([transitive-verb] "hits" "takes" "sees" "likes" "eats" "attacks" "defends" "is" "builds" "dances on the grave of" "beats")
    ([intransitive-verb] "dies" "eats" "goes" "thinks" "exists" "defends" "makes America great again")
    ([part-of-body] "hand" "arm" "foot" "leg" "neck" "head" "face" "torso" "crotch" "mouth" "eye")
    ([name] "Markus" "Donald Trump" "Squirt" "Socrates" "John McCarthy" "Patrick Volkerding")
    ([adjective] "magnificent" "disgusting" "happy" "tired" "cute" "strong" "weak" "big" "small" "ridiculous")
    ([adverb] "bravely" "foolishly" "quickly" "accidentally" "regularly" "finally" "barely" "sometimes" "often" "vigorously")
    ([preposition] "in" "on" "around" "through" "by" "over" "under" "about" "into")
    ([conjunction] "for" "and" "but" "or" "yet" "so")
    ([article] "a" "the")))

(defun random-element (x)
  (nth (random (length x)) x))

(defun a (vocab part-of-speech)
  (random-element (cdr (assoc part-of-speech vocab))))

(defun fill-in (part-of-speech &optional (vocab *default-vocabulary*))
  (if (assoc part-of-speech vocab)
    (a vocab part-of-speech)
    (string-downcase (string part-of-speech))))

(defmacro ad-lib (&rest rest)
  `(string-capitalize
    (reduce (lambda (x y) (concatenate 'string x " " y))
	    (mapcar #'fill-in (car '(,rest))))
    :end 1))

(defun ad-lib-structure (struct &optional (vocab *default-vocabulary*))
  (string-capitalize
    (reduce (lambda (x y) (concatenate 'string x " " y))
	    (mapcar (lambda (x) (fill-in x vocab)) struct))
    :end 1))

(defun one-in (x)
  (if (= (random x) 0) t nil))

(defun noun-phrase ()
  (let ((struct nil)(name (one-in 2)))
    (unless name (setf struct (concatenate 'list struct '([article]))))
    (loop until (one-in 4) do (setf struct (concatenate 'list struct '([adjective]))))
    (setf struct (concatenate 'list struct (if name '([name]) '([noun]))))))

(defun prepositional-phrase ()
  (concatenate 'list '([preposition]) (noun-phrase)))

(defun verb-phrase (&optional (transitive (one-in 2)))
  (let ((struct nil))
    (loop until (one-in 4) do (setf struct (concatenate 'list struct '([adverb]))))
    (setf struct (concatenate 'list struct (if transitive
					     (concatenate 'list '([transitive-verb])
							  (when (one-in 2) '([preposition]))
							  (noun-phrase))
					     (concatenate 'list '([intransitive-verb])
							  (when (one-in 2) (prepositional-phrase))))))))

(defun sentence-structure (&optional (struct nil))
  (concatenate 'list struct
	       (noun-phrase)
	       (verb-phrase (one-in 2))
	       (when (one-in 2) (prepositional-phrase))
	       (if (one-in 2) (concatenate 'list '([conjunction]) (sentence-structure)))))
