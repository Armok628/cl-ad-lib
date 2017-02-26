(setf *random-state* (make-random-state t))

(defparameter *default-vocabulary*
  '(([noun] "man" "ball" "woman" "table" "thing" "box" "wall" "bar" "place" "food" "dog" "animal" "computer")
    ([transitive-verb] "hits" "takes" "sees" "likes" "eats" "attacks" "defends" "is" "builds" "dances on the grave of" "beats")
    ([intransitive-verb] "dies" "eats" "goes" "thinks" "exists" "defends" "makes America great again")
    ([part-of-body] "hand" "arm" "foot" "leg" "neck" "head" "face" "torso" "crotch" "mouth" "eye" "cheek" "nose" "ear")
    ([name] "Markus" "Donald Trump" "Squirt" "Socrates" "John McCarthy" "Patrick Volkerding")
    ([adjective] "magnificent" "disgusting" "happy" "tired" "cute" "strong" "weak" "big" "small" "ridiculous")
    ([adverb] "bravely" "foolishly" "quickly" "accidentally" "regularly" "finally" "barely" "sometimes" "often" "vigorously")
    ([preposition] "in" "on" "around" "through" "by" "over" "under" "about" "into" "near" "with")
    ([conjunction] "for" "and" "but" "or" "yet" "so" "that")
    ([article] "a" "the")))

(defparameter *kids-vocabulary*
  '(([noun] "sock" "wheelbarrow" "toothpick" "happy meal" "dingleberry" "skateboard" "banana peel" "bug" "frog" "poop" "wipe")
    ([transitive-verb] "covers" "cooks" "gazes at" "chases" "plays with" "texts" "squeezes" "pokes" "tickles" "wipes")
    ([intransitive-verb] "breathes" "farts" "streaks" "dreams" "bathes" "swims" "runs" "plays" "hides")
    ([part-of-body] "bellybutton" "eye" "booty" "cheek" "pinky" "freckle" "nose" "ear" "ankle")
    ([name] "Daisy" "Mama" "Daddy" "Grandma" "Grandpa" "Uncle Mark" "Great-grandpa" "Squirt")
    ([adjective] "smelly" "crunchy" "hairy" "cheesy" "corroded" "smiling" "warm" "mashed" "dirty" "fresh")
    ([adverb] "deeply" "slowly" "accidentally" "hurriedly" "sometimes" "often" "vigorously" "finally" "barely" "narrowly")
    ([preposition] "in" "on" "around" "through" "by" "over" "under" "about" "into" "near" "with")
    ([conjunction] "for" "and" "but" "or" "yet" "so" "that")
    ([article] "a" "the")
    ([exclamation] "What in tarnation!?" "Shark!")))


(defun random-element (x)
  (nth (random (length x)) x))

(defun a (vocab part-of-speech)
  (random-element (cdr (assoc part-of-speech vocab))))

(defun fill-in (part-of-speech &optional (vocab *default-vocabulary*))
  (if (stringp part-of-speech) part-of-speech
  (if (assoc part-of-speech vocab)
    (a vocab part-of-speech)
    (string-downcase (string part-of-speech)))))

(defun ad-lib-structure (struct &optional (vocab *default-vocabulary*))
  (string-capitalize
    (reduce (lambda (x y) (concatenate 'string x " " y))
	    (mapcar (lambda (x) (fill-in x vocab)) struct))
    :end 1))

(defun replace-substring (subject target replacement &key (test #'char-equal))
  (let ((replace-pos (search target subject :test test)))
    (concatenate 'string
		 (subseq subject 0 replace-pos)
		 replacement
		 (subseq subject (+ replace-pos (length target))))))

(defun ad-lib-string (sentence &optional (vocab *default-vocabulary*))
  (loop for part-of-speech in vocab
  do (loop while (search (string (car part-of-speech)) sentence :test #'char-equal)
	   do (setf sentence (replace-substring
			       sentence
			       (string (car part-of-speech))
			       (fill-in (car part-of-speech) vocab)))))
  (string-capitalize sentence :end 1))

(defun custom-ad-lib (vocab &rest rest)
  (reduce (lambda (x y) (concatenate 'string x " " y)) (mapcar (lambda (x) (ad-lib-string x vocab)) rest)))

(defun ad-lib (&rest rest) (apply #'custom-ad-lib *default-vocabulary* rest))

(defun one-in (x)
  (when (= (random x) 0) t))

(defun noun-phrase ()
  (let ((struct nil)(name (one-in 2)))
    (unless name (setf struct (concatenate 'list struct '([article]))))
    (loop until (one-in 2) do (setf struct (concatenate 'list struct '([adjective]))))
    (setf struct (concatenate 'list struct (if name '([name]) '([noun]))))))

(defun prepositional-phrase ()
  (concatenate 'list '([preposition]) (noun-phrase)))

(defun verb-phrase (&optional (transitive (one-in 2)))
  (let ((struct nil))
    (loop until (one-in 2) do (setf struct (concatenate 'list struct '([adverb]))))
    (setf struct (concatenate 'list struct
			      (if transitive
				(concatenate 'list '([transitive-verb])
					     (when (one-in 2) '([preposition]))
					     (noun-phrase))
				'([intransitive-verb]))
			      (when (one-in 2) (prepositional-phrase))))))

(defun sentence-structure (&optional (struct nil))
  (concatenate 'list struct
	       (noun-phrase)
	       (verb-phrase (one-in 2)) (when (one-in 2) (prepositional-phrase))
	       (if (one-in 2) (concatenate 'list '([conjunction]) (sentence-structure)))))
