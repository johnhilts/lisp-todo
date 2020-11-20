(push #p"/home/jfh/code/lisp/source/util-lib/web/jfh-web/" asdf:*central-registry*)
(push #p"/home/jfh/code/lisp/source/util-lib/testing/jfh-testing/" asdf:*central-registry*)
(push #p"/home/jfh/code/lisp/source/util-lib/web/test/jfh-web-test/" asdf:*central-registry*)
(push #p"/home/jfh/code/lisp/source/web/todo/todo-project/" asdf:*central-registry*)

(asdf:load-system "jfh-web")
(asdf:load-system "jfh-testing")
(asdf:load-system "jfh-web-test")
(asdf:load-system "todo-project")
