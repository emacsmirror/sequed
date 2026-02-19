;;; sequed.el --- Major mode for FASTA and phylip/bpp DNA alignments -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025 Bruce Rannala

;; Author: Bruce Rannala <brannala@ucdavis.edu>
;; URL: https://github.com/brannala/sequed
;; Package-Version: 2.00
;; Package-Requires: ((emacs "25.2"))
;; License: GNU General Public License Version 3

;;; Commentary:

;; Major mode for editing DNA sequence data in FASTA and phylip/bpp
;; format and viewing multiple sequence alignments.
;;
;; Usage:
;;
;; M-x sequed-mode to invoke.  Automatically invoked as major mode for .fa and .aln files.
;; M-x sequed-phy-mode to invoke.  Automatically invoked as major mode for .phy files.
;; Sequed-mode:
;;   C-c C-r c  sequed-reverse-complement
;;   C-c C-r t  sequed-translate
;;   C-c C-e    sequed-export
;;   C-c C-a    sequed-mkaln
;;
;; Sequed-aln-mode:
;;   C-c C-b    sequed-aln-gotobase
;;   C-c C-f    sequed-aln-seqfeatures
;;   C-c C-k    sequed-aln-kill-alignment
;;
;; Sequed-phy-mode:
;;   C-c C-a    sequed-phy-mkaln
;;
;; Sequed-phy-aln-mode (inherits sequed-aln-mode bindings):
;;   C-c C-n    sequed-phy-aln-next-locus
;;   C-c C-p    sequed-phy-aln-prev-locus
;;   C-c C-g    sequed-phy-aln-goto-locus
;;   C-c C-x    sequed-phy-aln-extract-loci
;;   C-c C-b    sequed-aln-gotobase (inherited)
;;   C-c C-f    sequed-aln-seqfeatures (inherited)
;;   C-c C-k    sequed-aln-kill-alignment (inherited)

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:fa\\|aln\\)\\'" . sequed-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.phy\\'" . sequed-phy-mode))

(defface sequed-base-c '((t :background "green"))
  "Basic face for DNA base C."
  :group 'sequed)

(defface sequed-base-t '((t :background "orange"))
  "Basic face for DNA base T."
  :group 'sequed)

(defface sequed-base-a '((t :background "red"))
  "Basic face for DNA base A."
  :group 'sequed)

(defface sequed-base-g '((t :background "blue"))
  "Basic face for DNA base G."
  :group 'sequed)

(defvar sequed-aln-base-a 'sequed-base-a)
(defvar sequed-aln-base-c 'sequed-base-c)
(defvar sequed-aln-base-g 'sequed-base-g)
(defvar sequed-aln-base-t 'sequed-base-t)

(defvar sequed-aln-mode-font-lock nil
  "DNA base colors for `font-lock-defaults' in alignment view.")

(setq sequed-aln-mode-font-lock
      '(("^>[^\s]+" . font-lock-constant-face)
        ("[cC]" . sequed-aln-base-c)
        ("[tT]" . sequed-aln-base-t)
        ("[aA]" . sequed-aln-base-a)
        ("[gG]" . sequed-aln-base-g)))

(defconst sequed-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ; is a comment starter
    (modify-syntax-entry ?\; "<" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst sequed-colors
  '(("^[^>]\\([a-zA-Z- ]+\\)" . font-lock-string-face)
    ("^>.+\n" . font-lock-constant-face)))

(defvar sequed-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-a") #'sequed-mkaln)
    (define-key km (kbd "C-c C-e") #'sequed-export)
    (define-key km (kbd "C-c C-r c") #'sequed-reverse-complement)
    (define-key km (kbd "C-c C-r t") #'sequed-translate)
    (define-key km [menu-bar sequed]
      (cons "SequEd" (make-sparse-keymap "SequEd")))
    (define-key km [menu-bar sequed sequed-mkaln]
      '("View Alignment" . sequed-mkaln))
    (define-key km [menu-bar sequed sequed-export]
      '("Export" . sequed-export))
    (define-key km [menu-bar sequed sequed-reverse-complement]
      '("Reverse Complement" . sequed-reverse-complement))
    (define-key km [menu-bar sequed sequed-translate]
      '("Translation" . sequed-translate))
    km)
  "Keymap used in `sequed-mode'.")

;;;###autoload
(define-derived-mode sequed-mode fundamental-mode "SequEd"
  "Major mode for viewing and editing FASTA sequence data."
  :syntax-table sequed-mode-syntax-table
  (setq font-lock-defaults '(sequed-colors))
  (unless (sequed-check-fasta)
    (user-error "Not a FASTA file"))
  (font-lock-ensure)
  (sequed-get-sequence-length)
  (setq-local comment-start "; ")
  (setq-local comment-end ""))

(defvar sequed-aln-mode-map
  (let ((km2 (make-sparse-keymap)))
    (define-key km2 (kbd "C-c C-b") #'sequed-aln-gotobase)
    (define-key km2 (kbd "C-c C-f") #'sequed-aln-seqfeatures)
    (define-key km2 (kbd "C-c C-k") #'sequed-aln-kill-alignment)
    (define-key km2 [menu-bar sequedaln]
      (cons "SequEdAln" (make-sparse-keymap "SequEdAln")))
    (define-key km2 [menu-bar sequedaln move]
      '("Move to base" . sequed-aln-gotobase))
    (define-key km2 [menu-bar sequedaln features]
      '("Sequence features" . sequed-aln-seqfeatures))
    (define-key km2 [menu-bar sequedaln quit]
      '("Quit Alignment" . sequed-aln-kill-alignment))
    km2)
  "Keymap used in `sequed-aln-mode'.")

(define-derived-mode sequed-aln-mode fundamental-mode "SequEdAln"
  "Major mode for viewing sequence alignments."
  :syntax-table sequed-mode-syntax-table
  (setq font-lock-defaults '(sequed-aln-mode-font-lock))
  (font-lock-ensure)
  (setq mode-line-format
        (list "%e" mode-line-front-space mode-line-mule-info
              mode-line-client mode-line-modified
              mode-line-remote mode-line-frame-identification
              mode-line-buffer-identification
              "  SeqID:"
              '(:eval (aref sequed-seqID
                            (max 0 (1- (string-to-number (format-mode-line "%l"))))))
              " BasePos:"
              '(:eval (let* ((col (current-column))
                             (offset (- col (1- sequed-label-length))))
                        (when (> offset 0)
                          (number-to-string
                           (+ offset (1- sequed-startpos))))))
              "   "
              mode-line-modes mode-line-misc-info mode-line-end-spaces)))

(defun sequed-check-fasta ()
  "Check if current buffer looks like FASTA."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^>.+\n[ACGTNacgtn-]+" nil t)))

(defun sequed-remove-fasta-comments ()
  "Remove comment lines from current buffer (lines starting with `;')."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward "^;.*\n" nil t)
        (replace-match "" t t)))))

(defvar-local sequed-label-length nil)
(defvar-local sequed-seq-length nil)
(defvar-local sequed-noseqs nil)
(defvar-local sequed-seqID nil)
(defvar-local sequed-startpos nil)
(defvar-local sequed-endpos nil)

(defun sequed--parse-fasta-buffer ()
  "Parse current buffer as FASTA.

Return a list of (LABEL . SEQ) where LABEL includes the leading `>'."
  (save-excursion
    (goto-char (point-min))
    (let (result label seq-start)
      (while (re-search-forward "^>\\([^\n]*\\)\n" nil t)
        (setq label (match-string 0))
        (setq seq-start (point))
        (let ((seq-end (or (and (re-search-forward "^>" nil t)
                                (match-beginning 0))
                           (point-max))))
          (let* ((raw (buffer-substring-no-properties seq-start seq-end))
                 (seq (replace-regexp-in-string "[ \t\n\r]" "" raw)))
            (push (cons label seq) result))
          (goto-char seq-end)))
      (nreverse result))))

(defun sequed-get-sequence-length ()
  "Set `sequed-seq-length' from first FASTA sequence in buffer."
  (let* ((pairs (sequed--parse-fasta-buffer))
         (first (car pairs)))
    (setq sequed-seq-length (length (cdr first)))))

(defun sequed-labels-equal-length (labels)
  "Pad LABELS so all have equal length plus one space."
  (let* ((trimmed (mapcar #'string-trim labels))
         (maxlen (cl-loop for s in trimmed maximize (length s))))
    (cl-loop for s in trimmed
             collect (concat s
                             (make-string (1+ (- maxlen (length s))) ?\s)))))

(defun sequed-short-labels (labels)
  "Create short LABELS for display in mode line."
  (let* ((small-size (min 11 (length (nth 1 labels))))
         (len (length labels))
         (vec (make-vector len "Empty")))
    (cl-loop for i from 0 below len do
             (aset vec i (substring (nth i labels) 1 small-size)))
    vec))

(defun sequed-color-labels ()
  "Identify labels and color them."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward "^>[[:word:]\-/|_.]+" nil t)
        (add-text-properties
         (match-beginning 0) (match-end 0)
         '(face (:foreground "yellow")))))))

(defun sequed-mkaln (startpos endpos)
  "Create read-only buffer for alignment viewing from STARTPOS to ENDPOS."
  (interactive
   (let ((spos (read-number "Start Pos: " 1)))
     (sequed-get-sequence-length)
     (let ((epos (read-number "End Pos: " sequed-seq-length)))
       (list spos epos))))
  (unless (sequed-check-fasta)
    (user-error "Not a FASTA file"))
  (when (or (< startpos 1)
            (> endpos sequed-seq-length)
            (>= startpos endpos))
    (user-error "Invalid start/end positions"))
  (let* ((orig-buf (current-buffer))
         (pairs (with-current-buffer orig-buf
                  (save-excursion
                    (sequed-remove-fasta-comments)
                    (sequed--parse-fasta-buffer))))
         (labels (mapcar #'car pairs))
         (seqs   (mapcar #'cdr pairs))
         (nseqs  (length seqs))
         (elabels (sequed-labels-equal-length labels))
         (trimmed
          (cl-loop for s in seqs
                   collect (substring s (1- startpos) endpos)))
         (lines
          (cl-loop for lab in elabels
                   for s in trimmed
                   collect (concat lab s)))
         (buf (get-buffer-create "*Alignment Viewer*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sequed-aln-mode)
        (setq truncate-lines t)
        (setq sequed-label-length (length (car elabels)))
        (setq sequed-seq-length (length (car seqs)))
        (setq sequed-noseqs nseqs)
        (setq sequed-seqID (sequed-short-labels labels))
        (setq sequed-startpos startpos)
        (setq sequed-endpos endpos)
        (insert (mapconcat #'identity lines "\n"))
        (insert "\n")
        (sequed-color-labels)
        (read-only-mode 1)))
    (display-buffer buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (sequed-aln-gotobase sequed-startpos))))

(defun sequed-aln-gotobase (position)
  "Move to base at POSITION in sequence that cursor is positioned in."
  (interactive "nPosition of base: ")
  (when (or (< position sequed-startpos)
            (> position sequed-endpos))
    (user-error "Attempt to move to base outside sequence"))
  (beginning-of-line)
  (move-to-column (- (+ position (1- sequed-label-length))
                     (1- sequed-startpos))))

(defun sequed-aln-seqfeatures ()
  "List number of sequences and length of region."
  (interactive)
  (message "Sequences:%d Sites:%d" sequed-noseqs sequed-seq-length))

(defun sequed-aln-kill-alignment ()
  "Kill alignment buffer and window."
  (interactive)
  (kill-buffer-and-window))

(defun sequed-export ()
  "Export alignment in PHYLIP-like format for phylogenetic software."
  (interactive)
  (let* ((oldbuf (current-buffer))
         nseqs nsites f-lines templine f-buffer)
    (save-current-buffer
      (set-buffer (get-buffer-create "*export alignment*"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring-no-properties oldbuf)
        (setq f-buffer (buffer-substring-no-properties (point-min) (point-max)))
        (goto-char (point-min))
        (setq f-lines
              (split-string f-buffer ">\\([[:word:]\-/|_.]+\\)\\([\s]+.*\n\\)?" t))
        (setq templine (mapconcat #'concat (split-string (nth 1 f-lines) "\n" t) ""))
        (setq-local nsites (length templine))
        (goto-char (point-min))
        (setq-local comment-start "; ")
        (setq-local comment-end "")
        (sequed-remove-fasta-comments)
        (goto-char (point-min))
        (setq nseqs (how-many ">[[:word:]\-/|_.]+"))
        (goto-char (point-min))
        (while (re-search-forward ">[[:word:]\-/|_.]+\\([^\n]*\\)\n" nil t)
          (replace-match "" t t))
        (goto-char (point-min))
        (insert (format "%d  %d\n" nseqs nsites))
        (while (re-search-forward ">" nil t)
          (delete-char 1))
        (fundamental-mode)
        (display-buffer (current-buffer))))))

(defvar sequed-genetic-code-universal (make-hash-table :test 'equal))

;; Genetic code table (unchanged)
(mapc (lambda (pair)
        (puthash (car pair) (cdr pair) sequed-genetic-code-universal))
      '(("ttt" . ?F) ("ttc" . ?F) ("tta" . ?L) ("ttg" . ?L)
        ("tct" . ?S) ("tcc" . ?S) ("tca" . ?S) ("tcg" . ?S)
        ("taa" . ?*) ("tag" . ?*) ("tat" . ?Y) ("tac" . ?Y)
        ("tgt" . ?C) ("tgc" . ?C) ("tga" . ?*) ("tgg" . ?W)
        ("ctt" . ?L) ("ctc" . ?L) ("cta" . ?L) ("ctg" . ?L)
        ("cct" . ?P) ("ccc" . ?P) ("cca" . ?P) ("ccg" . ?P)
        ("cat" . ?H) ("cac" . ?H) ("caa" . ?Q) ("cag" . ?Q)
        ("cgt" . ?R) ("cgc" . ?R) ("cga" . ?R) ("cgg" . ?R)
        ("att" . ?I) ("atc" . ?I) ("ata" . ?I) ("atg" . ?M)
        ("act" . ?T) ("acc" . ?T) ("aca" . ?T) ("acg" . ?T)
        ("aat" . ?N) ("aac" . ?N) ("aaa" . ?K) ("aag" . ?K)
        ("agt" . ?S) ("agc" . ?S) ("aga" . ?R) ("agg" . ?R)
        ("gtt" . ?V) ("gtc" . ?V) ("gta" . ?V) ("gtg" . ?V)
        ("gct" . ?A) ("gcc" . ?A) ("gca" . ?A) ("gcg" . ?A)
        ("gat" . ?D) ("gac" . ?D) ("gaa" . ?E) ("gag" . ?E)
        ("ggt" . ?G) ("ggc" . ?G) ("gga" . ?G) ("ggg" . ?G)))

(defun sequed-translate (seqbegin seqend)
  "Translation of marked DNA region SEQBEGIN SEQEND into amino acids."
  (interactive "r")
  (let* ((raw (buffer-substring-no-properties seqbegin seqend))
         (x   (replace-regexp-in-string "[ \t\n\r]" "" raw))
         (len (length x))
         (codons (/ len 3))
         (y    (make-string codons ?.))
         (i 0) (j 0))
    (while (<= (+ i 2) (1- len))
      (let ((codon (downcase (substring x i (+ i 3)))))
        (aset y j (or (gethash codon sequed-genetic-code-universal) ?.)))
      (setq i (+ i 3))
      (setq j (1+ j)))
    (let ((buf (generate-new-buffer "*translation*")))
      (with-current-buffer buf
        (insert y))
      (switch-to-buffer buf))))

(defun sequed-basepair (base)
  "Find the complement of a DNA BASE."
  (pcase base
    (?a ?t) (?t ?a) (?c ?g) (?g ?c)
    (?A ?T) (?T ?A) (?C ?G) (?G ?C)
    (_ base)))

(defun sequed-reverse-complement (seqbegin seqend)
  "Get reverse-complement of marked region SEQBEGIN SEQEND in new buffer."
  (interactive "r")
  (let* ((raw (buffer-substring-no-properties seqbegin seqend))
         (x   (replace-regexp-in-string "[ \t\n\r]" "" raw))
         (len (length x))
         (out (make-string len ?N)))
    (cl-loop for i from 0 below len
             for b = (aref x i)
             for c = (sequed-basepair b)
             do (aset out (- (1- len) i) c))
    (let ((buf (generate-new-buffer "*reverse complement*")))
      (with-current-buffer buf
        (insert out))
      (switch-to-buffer buf))))

;;; Phylip/BPP mode

(defface sequed-phy-nseqs '((t :foreground "cyan" :weight bold))
  "Face for number of sequences in phylip/bpp header line."
  :group 'sequed)

(defface sequed-phy-nsites '((t :foreground "magenta" :weight bold))
  "Face for number of sites in phylip/bpp header line."
  :group 'sequed)

(defface sequed-phy-label '((t :foreground "yellow"))
  "Face for sequence labels in phylip/bpp format."
  :group 'sequed)

(defvar sequed-phy-font-lock
  '(("^\\s-*\\([0-9]+\\)\\s-+\\([0-9]+\\)\\s-*$"
     (1 'sequed-phy-nseqs)
     (2 'sequed-phy-nsites))
    ("^\\([[:alpha:]][[:graph:]]*\\)"
     (1 'sequed-phy-label)))
  "Font-lock keywords for `sequed-phy-mode'.")

(defvar sequed-phy-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-a") #'sequed-phy-mkaln)
    (define-key km [menu-bar sequedphy]
      (cons "SequEdPhy" (make-sparse-keymap "SequEdPhy")))
    (define-key km [menu-bar sequedphy sequed-phy-mkaln]
      '("View Alignment" . sequed-phy-mkaln))
    km)
  "Keymap used in `sequed-phy-mode'.")

;;;###autoload
(define-derived-mode sequed-phy-mode fundamental-mode "SequEdPhy"
  "Major mode for viewing and editing phylip/bpp format sequence data."
  (setq font-lock-defaults '(sequed-phy-font-lock))
  (font-lock-ensure))

;;; Phylip/BPP alignment viewer

(defvar-local sequed-phy-loci nil
  "Vector of parsed locus plists for the phylip alignment viewer.")
(put 'sequed-phy-loci 'permanent-local t)
(defvar-local sequed-phy-locus-index 0
  "0-based index of currently displayed locus.")
(put 'sequed-phy-locus-index 'permanent-local t)
(defvar-local sequed-phy-locus-count 0
  "Total number of loci.")
(put 'sequed-phy-locus-count 'permanent-local t)
(defvar-local sequed-phy-source nil
  "Name of the source .phy buffer.")
(put 'sequed-phy-source 'permanent-local t)

(defun sequed--parse-phy-buffer ()
  "Parse current buffer as multi-locus phylip format.
Return a vector of plists, each (:nseqs N :nsites M :labels LIST :seqs LIST).
Assumes sequential (non-interleaved) format with one sequence per line."
  (save-excursion
    (goto-char (point-min))
    (let (loci)
      (while (re-search-forward
              "^\\s-*\\([0-9]+\\)\\s-+\\([0-9]+\\)\\s-*$" nil t)
        (let* ((nseqs (string-to-number (match-string 1)))
               (nsites (string-to-number (match-string 2)))
               labels seqs)
          (forward-line 1)
          (dotimes (_ nseqs)
            (when (looking-at
                   "^\\([[:alpha:]][[:graph:]]*\\)\\s-+\\(.*\\)$")
              (push (match-string-no-properties 1) labels)
              (push (string-trim (match-string-no-properties 2)) seqs))
            (forward-line 1))
          (push (list :nseqs nseqs :nsites nsites
                      :labels (nreverse labels) :seqs (nreverse seqs))
                loci)))
      (vconcat (nreverse loci)))))

(defun sequed-phy-short-labels (labels)
  "Create short LABELS for mode-line display (no > prefix)."
  (let* ((small-size (min 10 (length (car labels))))
         (len (length labels))
         (vec (make-vector len "Empty")))
    (cl-loop for i from 0 below len do
             (let ((lab (nth i labels)))
               (aset vec i (substring lab 0
                                      (min small-size (length lab))))))
    vec))

(defun sequed-phy-color-labels ()
  "Identify and color phylip sequence labels."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward "^[[:alpha:]][[:graph:]]*" nil t)
        (add-text-properties
         (match-beginning 0) (match-end 0)
         '(face (:foreground "yellow")))))))

(defun sequed-phy-color-bases ()
  "Apply DNA base colors only to the sequence portion of each line.
Skips the label region (columns 0 to `sequed-label-length')."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (not (eobp))
        (let ((seq-start (+ (line-beginning-position) sequed-label-length))
              (line-end (line-end-position)))
          (when (< seq-start line-end)
            (goto-char seq-start)
            (while (< (point) line-end)
              (pcase (downcase (char-after))
                (?a (put-text-property (point) (1+ (point))
                                       'face 'sequed-base-a))
                (?t (put-text-property (point) (1+ (point))
                                       'face 'sequed-base-t))
                (?c (put-text-property (point) (1+ (point))
                                       'face 'sequed-base-c))
                (?g (put-text-property (point) (1+ (point))
                                       'face 'sequed-base-g)))
              (forward-char 1))))
        (forward-line 1)))))

(defun sequed-phy--render-locus (locus startpos endpos)
  "Render LOCUS plist in current buffer from STARTPOS to ENDPOS."
  (let* ((labels (plist-get locus :labels))
         (seqs (plist-get locus :seqs))
         (nseqs (plist-get locus :nseqs))
         (elabels (sequed-labels-equal-length labels))
         (trimmed (cl-loop for s in seqs
                           collect (substring s (1- startpos) endpos)))
         (lines (cl-loop for lab in elabels
                         for s in trimmed
                         collect (concat lab s)))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (mapconcat #'identity lines "\n"))
    (insert "\n")
    (setq sequed-label-length (length (car elabels)))
    (setq sequed-seq-length (plist-get locus :nsites))
    (setq sequed-noseqs nseqs)
    (setq sequed-seqID (sequed-phy-short-labels labels))
    (setq sequed-startpos startpos)
    (setq sequed-endpos endpos)
    (sequed-phy-color-labels)
    (sequed-phy-color-bases)
    (goto-char (point-min))))

(defun sequed-phy-mkaln ()
  "Create alignment viewer for phylip/bpp format data."
  (interactive)
  (let* ((source-buf (buffer-name))
         (loci (sequed--parse-phy-buffer))
         (nloci (length loci))
         (buf (get-buffer-create "*Phy Alignment Viewer*")))
    (when (= nloci 0)
      (user-error "No loci found in buffer"))
    ;; Activate mode in its own block so hooks cannot shift current-buffer
    ;; away from buf before the variable assignments below.
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (sequed-phy-aln-mode)))
    ;; Set state and render in a fresh block guaranteed to be in buf.
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (setq truncate-lines t
              sequed-phy-loci loci
              sequed-phy-locus-index 0
              sequed-phy-locus-count nloci
              sequed-phy-source source-buf)
        (sequed-phy--render-locus (aref loci 0) 1
                                  (plist-get (aref loci 0) :nsites))
        (read-only-mode 1)))
    (display-buffer buf)
    (with-current-buffer buf
      (goto-char (point-min)))))

(defun sequed-phy-aln-next-locus ()
  "Display next locus in alignment viewer."
  (interactive)
  (if (>= sequed-phy-locus-index (1- sequed-phy-locus-count))
      (message "Already at last locus")
    (cl-incf sequed-phy-locus-index)
    (let ((locus (aref sequed-phy-loci sequed-phy-locus-index)))
      (sequed-phy--render-locus locus 1 (plist-get locus :nsites))
      (read-only-mode 1))))

(defun sequed-phy-aln-prev-locus ()
  "Display previous locus in alignment viewer."
  (interactive)
  (if (<= sequed-phy-locus-index 0)
      (message "Already at first locus")
    (cl-decf sequed-phy-locus-index)
    (let ((locus (aref sequed-phy-loci sequed-phy-locus-index)))
      (sequed-phy--render-locus locus 1 (plist-get locus :nsites))
      (read-only-mode 1))))

(defun sequed-phy-aln-goto-locus (n)
  "Jump to locus N (1-based)."
  (interactive "nGoto locus: ")
  (if (or (< n 1) (> n sequed-phy-locus-count))
      (user-error "Locus %d out of range (1-%d)" n sequed-phy-locus-count)
    (setq sequed-phy-locus-index (1- n))
    (let ((locus (aref sequed-phy-loci sequed-phy-locus-index)))
      (sequed-phy--render-locus locus 1 (plist-get locus :nsites))
      (read-only-mode 1))))

(defun sequed-phy-aln-extract-loci (from to)
  "Extract loci FROM to TO (1-based, inclusive) into a new buffer.
The new buffer contains valid phylip format and activates `sequed-phy-mode'."
  (interactive "nFrom locus: \nnTo locus: ")
  (when (or (< from 1) (> to sequed-phy-locus-count) (> from to))
    (user-error "Invalid locus range %d-%d (1-%d)"
                from to sequed-phy-locus-count))
  (let ((loci sequed-phy-loci)
        (buf (get-buffer-create "*Extracted Loci*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (cl-loop for i from (1- from) below to do
                 (let* ((locus (aref loci i))
                        (nseqs (plist-get locus :nseqs))
                        (nsites (plist-get locus :nsites))
                        (labels (plist-get locus :labels))
                        (seqs (plist-get locus :seqs))
                        (elabels (sequed-labels-equal-length labels)))
                   (insert (format "%d  %d\n" nseqs nsites))
                   (cl-loop for lab in elabels
                            for s in seqs
                            do (insert lab s "\n"))
                   (insert "\n")))
        (sequed-phy-mode)))
    (display-buffer buf)))

(defvar sequed-phy-aln-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-n") #'sequed-phy-aln-next-locus)
    (define-key km (kbd "C-c C-p") #'sequed-phy-aln-prev-locus)
    (define-key km (kbd "C-c C-g") #'sequed-phy-aln-goto-locus)
    (define-key km (kbd "C-c C-x") #'sequed-phy-aln-extract-loci)
    ;; Suppress parent SequEdAln menu; our menu replaces it
    (define-key km [menu-bar sequedaln] 'undefined)
    (define-key km [menu-bar sequedphyaln]
      (cons "SequEdPhyAln" (make-sparse-keymap "SequEdPhyAln")))
    (define-key km [menu-bar sequedphyaln next]
      '("Next locus" . sequed-phy-aln-next-locus))
    (define-key km [menu-bar sequedphyaln prev]
      '("Previous locus" . sequed-phy-aln-prev-locus))
    (define-key km [menu-bar sequedphyaln goto]
      '("Goto locus" . sequed-phy-aln-goto-locus))
    (define-key km [menu-bar sequedphyaln extract]
      '("Extract loci" . sequed-phy-aln-extract-loci))
    (define-key km [menu-bar sequedphyaln move]
      '("Move to base" . sequed-aln-gotobase))
    (define-key km [menu-bar sequedphyaln features]
      '("Sequence features" . sequed-aln-seqfeatures))
    (define-key km [menu-bar sequedphyaln quit]
      '("Quit Alignment" . sequed-aln-kill-alignment))
    km)
  "Keymap used in `sequed-phy-aln-mode'.")

(define-derived-mode sequed-phy-aln-mode sequed-aln-mode "SequEdPhyAln"
  "Major mode for viewing phylip/bpp sequence alignments.
Derives from `sequed-aln-mode' to inherit DNA base coloring,
mode-line (SeqID + BasePos), and keybindings.  Adds locus navigation."
  (font-lock-mode -1)
  (setq mode-line-format
        (list "%e" mode-line-front-space mode-line-mule-info
              mode-line-client mode-line-modified
              mode-line-remote mode-line-frame-identification
              mode-line-buffer-identification
              "  SeqID:"
              '(:eval (aref sequed-seqID
                            (max 0 (1- (string-to-number
                                        (format-mode-line "%l"))))))
              " BasePos:"
              '(:eval (let* ((col (current-column))
                             (offset (- col (1- sequed-label-length))))
                        (when (> offset 0)
                          (number-to-string
                           (+ offset (1- sequed-startpos))))))
              " Locus:"
              '(:eval (format "%d/%d"
                              (1+ sequed-phy-locus-index)
                              sequed-phy-locus-count))
              "   "
              mode-line-modes mode-line-misc-info mode-line-end-spaces)))

(provide 'sequed)

;;; sequed.el ends here
