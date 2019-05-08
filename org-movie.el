;;; org-movie.el --- An org-mode movie watch list utility

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The function org-movie-from-imdb will be useful to most folk
;; Requires an API key from https://www.omdbapi.com, of which the authors
;; of the this program disavow any responsibility for
;;
;; Enter your key in the function definition for movie-from-imdb
;; Credits:

;; This program is a lightly modified version of https://gist.github.com/bo0ts/4290237
;;; Code:

;; Org_movie

(provide 'org-movie)

(require 'org)

(require 'url)

(defcustom org-movie-omdb-apikey nil
  "Your API key for the OMDb API, which can be registered at `https://www.omdbapi.com/'."
  :type 'string)

(defun get-and-parse-json (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (json-read)))

(defun get-movie-json (title &optional year)
  (let ((url (concat "http://omdbapi.com/?t=" (url-hexify-string
                                               title))))
    (if (consp year)
        (get-and-parse-json (concat url "&y=" (number-to-string year)))
      (get-and-parse-json url))))

(defun get-poster (url id folder)
  (let ((filename (concat (file-name-as-directory folder) id ".jpg")))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (kill-region (point-min) (point))
      (kill-line) ;; necessary to get rid of the empty line
      (write-file (concat (file-name-as-directory folder) id ".jpg") nil)
      )
    filename))


;; api ref: https://media.readthedocs.org/pdf/omdbpy/latest/omdbpy.pdf
(defun org-from-imdb-json (jsonmovie)
  (let* ((imdbID (cdr (assoc 'imdbID jsonmovie)))
         (poster (cdr (assoc 'Poster jsonmovie))))
    (concat "[[https://www.imdb.com/title/" imdbID "][" (cdr (assoc 'Title jsonmovie)) "]] [" (cdr (assoc 'Year jsonmovie)) "] " (tags-from-genre jsonmovie) "\n"
            ":PROPERTIES:\n"
            ":Director: " (cdr (assoc 'Director jsonmovie)) "\n"
            ":Year: " (cdr (assoc 'Year jsonmovie)) "\n"
            ":Actors: " (cdr (assoc 'Actors jsonmovie)) "\n"
            ":Genre: " (cdr (assoc 'Genre jsonmovie)) "\n"
            ":Plot: " (cdr (assoc 'Plot jsonmovie)) "\n"
            ":Runtime: " (cdr (assoc 'Runtime jsonmovie)) "\n"
            ;; (if (string-equal poster "N/A")
            ;;     (identity ":Poster: N/A\n" )
            ;;   (concat ":Poster: " "[[" (get-poster poster imdbID "~/moviedb/posters/") "]]" "\n"))
            ":END:"
            )))

(defun tags-from-genre (jsonmovie)
  (replace-regexp-in-string "-" ""
                            (upcase (concat ":" (replace-regexp-in-string ", " ":" (cdr (assoc 'Genre jsonmovie))) ":")))
  )

(defun org-movie-from-imdb (imdburl)
  (interactive "sUrl: ")
  (let ((imdbid (progn
                  (string-match "http[s]?://www.imdb.com/title/\\([^/]*\\)" imdburl)
                  (match-string 1 imdburl))))
    (save-excursion (insert (org-from-imdb-json (get-and-parse-json
                                                 (concat "http://omdbapi.com/?i=" imdbid "&apikey=" org-movie-omdb-apikey)))))
    (org-mark-element)
    (indent-region (region-beginning) (region-end))
    (org-align-all-tags)))
