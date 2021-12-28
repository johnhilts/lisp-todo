(in-package #:todo-project)

(defparameter *system-settings-file-path* "./system-settings-list.sexp")

(defparameter *users-root-folder-path* "./users")
(defparameter *user-index* nil)

(defvar *todo-api-endpoint*  "/todo-data")
(defvar *app-settings-api-endpoint*  "/app-settings-data")
(defvar *app-settings-file-name* "app-settings-list.sexp")
(defvar *todo-file-name* "todo-list.sexp")
(defvar *web-settings-file-path* "./web-settings.sexp")

(defvar *recipe-api-endpoint*  "/recipe-data")
(defvar *recipe-file-path* "./recipe-list.sexp")
