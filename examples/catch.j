;
; catching tests
;
                class           "Catch", "java/lang/Object",
                flags           public, super,
                version         0, 49,
                source          "catch.j"

<init>:         method          "()V"
                stack           0x01
                locals          0x01
                aload_0
                invokespecial   [method_ref "java/lang/Object", "<init>", "()V"]
                return

main:           method          "([Ljava/lang/String;)V"
                flags           public, static,
                stack           0x02
                locals          0x01
                invokestatic    [method_ref this, "panic", "()V"]
                ;invokevirtual   [method_ref "java/lang/Object", "toString", "()Ljava/lang/String;"]
                ;getstatic       [field_ref "java/lang/System", "out", "Ljava/io/PrintStream;"]
                ;swap
                ;invokevirtual   [method_ref "java/io/PrintStream", "println", "(Ljava/lang/String;)V"]
                return

panic:          method          "()V"
                flags           static
                stack           0x03
                locals          0x00
                new             "java/lang/Exception"
                dup
                ldc             [string "panic!!"]
                invokespecial   [method_ref "java/lang/Exception", "<init>", "(Ljava/lang/String;)V"]
                athrow
                return

dontPanic:      method          "()Ljava/lang/Exception;"
                flags           static
                stack           0x03
                locals          0x00
try_start:      invokestatic    [method_ref this, "panic", "()V"]
try_end:        aconst_null
catch_start:    areturn
                catch           try_start, try_end, catch_start, "java/lang/Exception"
