            class           "Hello", "java/lang/Object",
            flags           public, super, final, synthetic
            version         0, 51,

<init>:     method          "()V"
            stack           0x1
            locals          0x1
            aload_0
            invokespecial   [method_ref "java/lang/Object", "<init>", "()V"]
            return

main:       method          "([Ljava/lang/String;)V"
            flags           public, static, final
            stack           0x2
            locals          0x1
            getstatic       [field_ref "java/lang/System", "out", "Ljava/io/PrintStream;"]
            ldc             [string "Hello, World!"]
            invokevirtual   [method_ref "java/io/PrintStream", "println", "(Ljava/lang/String;)V"]
            return