;; sexml-tags.lisp


(in-package :sexml-objects)

(defpackage #:<)

(contextl:ensure-active-layer 'widget-classes)

(sexml:with-compiletime-active-layers (sexml:standard-sexml sexml-objects::widget-classes)
  (sexml:support-dtd "/root/quicklisp/local-projects/html5.dtd" :<))