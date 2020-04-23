;; calculeaza factorialul unui numar dat ca parametru
(defun fact (n)
	(cond
		((zerop n) 1)
		(T (* n (fact (- n 1))))
	)
)

;; inverseaza o lista data ca si argument (fara a folosi reverse)
(defun rev (l)
	(rev_acc l nil)
)

(defun rev_acc (l rez)
	(cond
		((endp l) rez)
		(T (rev_acc (rest l) (cons (car l) rez)))
	)
)

;; calculeaza lungimea unei liste data ca argument (fara a folosi length)
(defun len (l)
	(cond
		((endp l) 0)
		(T (+ 1 (len (rest l))))
	)
)

;; calculeaza numarul de atomi dintr-o lista imbricata
(defun nb_atomi (l)
	(cond
		((endp l) 0)
		((atom (car l)) (+ 1 (nb_atomi (rest l))))
		(T (+ (nb_atomi (car l)) (nb_atomi (rest l))))
	)
)

;; verifica daca lista data ca parametru este ordonata crescator
(defun cresc (l)
	(cond
		((equal (length l) 1) T)
		(T (setq a (car l))
		   (setq b (cadr l))
		   (cond
		   	((< a b) (cresc (rest l)))
		   	(T nil)
		   	)
		 )
	)
)

;; verifica daca lista data ca parametru este ordonata descrescator
(defun descresc (l)
	(cond
		((equal (length l) 1) T)
		(T (cond
			((> (car l) (cadr l)) (descresc (rest l)))
			(T NIL)
		))
	)
)

;; calculeaza maximul unei liste (fara a folosi max sau min)
(defun maxim (l)
	(cond
		((equal (length l) 1) (car l))
		(T (cond
			((> (car l) (maxim2 (rest l))) (car l))
			(T (maxim2 (rest l)))
			)
		)
	)
)

;; calculeaza minimul unei liste (fara a folosi max sau min)
(defun minim (l)
	(cond
		((equal (length l) 1) (car l))
		(T (cond
			((< (car l) (minim (rest l))) (car l))
			(T (minim (rest l)))
		))
	)
)

;; verifica daca o lista este simetrica fata de mijlocul ei/palindrom (eg. (1 2 3 2 1), (1 2 3 3 2 1))
(defun palindrom (l)
	(cond
		((equal l (reverse l)) T)
		(T nil)
	)
)

;; concateneaza doua sau mai multe liste date ca argumente (fara a folosi append)
(defun concat2 (l1 l2)
	(cond
		((endp l1) l2)
		((endp l2) l1)
		(T (cons (car l1) (concat2 (rest l1) l2)))
	)
)

(defun concat (l)
	(cond 
		((endp l) nil)
		(T (concat2 (car l) (concat (rest l))))
	)
)

;; calculeaza lungimea maxima a sublistelor unei liste
(defun maxsub (l)
	(cond 
		((atom l) 0)
		(T (max (length l)
				(maxsub (car l))
				(maxsub (rest l)))
		)
	)
)

;; returneaza radical(p*(p-a)*(p-b)*(p-c)), unde p=(a+b+c)/c (functia are ca argumente a, b si c)
(defun heron (a b c)
	(setq p (/ (+ a b c) 2))
	(setq rez (sqrt (* p (- p a) (- p b) (- p c))))
)


;; verifica daca elementele listei l date ca argument sunt divizibile cu y (alt argument)
(defun diviz (l y)
	(cond
		((endp l) T)
		((atom (car l))
			(cond 
				((zerop (mod (car l) y)) (diviz (rest l) y))
				(T nil)
			))
			(T (and (diviz (car l) y)
					(diviz (rest l) y)))
	)
)

;; verifica daca toate elementele unei liste sunt numere pare
(defun pare (l)
	(cond
		((endp l) T)
		((atom (car l))
			(cond
				((evenp (car l)) (pare (rest l)))
			)
		)
		(T (and (pare (car l)) (pare (rest l))))
	)
)

;; calculeaza media aritmetica a valorilor unei liste data ca parametru
(defun media (l)
	(/ (suma l) (length l))
)

(defun suma (l)
	(cond
		((endp l) 0)
		(T (+ (car l) (suma (rest l))))
	)
)

;; verifica daca doua liste sunt egale (fara a folosi equal)
(defun listegale (l1 l2)
	(cond
		((and (endp l1) (endp l2)) T)
		((endp l1) nil)
		((endp l2) nil)
		(T
		(cond
			((= (car l1) (car l2)) (listegale (rest l1) (rest l2)))
			(T nil)
		))
	)
)

;; verifica daca o lista are elemente duplicate
(defun duplicate (l)
	(cond
		((endp l) nil)
		(T (cond
			((member (car l) (rest l)) T)
			(T (duplicate (rest l)))
		))
	)
)








