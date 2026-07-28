

(clear-all)

(define-model test-imaginal-action
    
    (sgp-fct (list :style-warnings nil :v *out-file* :do-not-harvest 'imaginal))
  
  (chunk-type simple count)
  
  (p start
     ?imaginal>
     state free
     buffer empty
     error nil
     ==>
     +imaginal-action>
     isa simple-action
     action "make-simple-chunk")
  
  
  (p increment
     =imaginal>
     isa simple
     < count 4
     ?imaginal>
     state free
     ==>
     +imaginal-action>
     isa generic-action
     action "update-imaginal"
     slots (count))
  
    (p dont-increment
     =imaginal>
     isa simple
     count 4
     ?imaginal>
     state free
     ==>
     +imaginal-action>
     isa simple-action
     action "no-chunk-generated")
  
  (p stop
     ?imaginal>
     error t
     ==>
     +imaginal>)
  )