;
; define the class jas.example.Hello
;
            class           "jas/example/Hello", "java/lang/Object"
            impl            "jas/example/Foo"
            flags           public, super
            version         0, 51

System:     class_ref       "java/lang/System"                      ; declare a class ref constant

out:        field_ref       System, "out", "Ljava/io/PrintStream;"  ; reference another constant
err:        field_ref       0x1, "out", "Ljava/io/PrintStream;"     ; or be obnoxious and use a numeric pointer
                                                                    ; constants will appear in the order they
                                                                    ; are declared
println:    method_ref      "java/io/PrintStream", "println", "(Ljava/lang/String;)V"
string:     string          "string"                                ; we can name the constant "string", since it's
                                                                    ; in label position, not type position
            int             42                                      ; declare an anonymous constant
                                                                    ; we know to start a new constant since it's on
                                                                    ; a new line without a preceding label
            field_ref       0x1, [name_and_type "in", "Ljava/io/InputStream;"]

foo:        field           "I"         ; the field instructon declates a new field
            flags           public      ; the flags instruction applies flags to the
                                        ; current field/method/class being defined

<init>:     method          "()V"
            flags           public
            aload_0
            invokespecial   [method_ref "java/lang/Object", "<init>", "()V"]
            return

main:       method          "([Ljava/lang/String;)V"
            flags           public, static
            getstatic       out
            ldc             [string "Hello, world!"] ; create a constant and return its address
            invokevirtual   println
            return

inst_str:   string          "Hello, instance!"

instance:   method          "()V"
            flags           public
            getstatic       out
            ldc             inst_str
            invokevirtual   println
            return

instance_by_num:
            method          "()V"   ; legal, since there's a label that hasn't
                                    ; found an instruction yet
            flags           public
            getstatic       out
            ldc             0x4     ; point directly to a constant
            invokevirtual   println
            return

loopForever:
            method          "()V"
            flags           public, static
start:      goto            start ; reference a local label
            return

getFoo:     method          "()I"
            flags           public
            aload_0
            getfield        [field_ref this, "foo", "I"]
            ireturn

;
; define the interface jas.example.Foo
;
            class           "jas/example/Foo", "java/lang/Object"
            flags           public, interface
            version         51.0