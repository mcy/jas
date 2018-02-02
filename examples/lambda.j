            class           "Lambda", "java/lang/Object",
            flags           public, super, final, synthetic
            version         0, 51,

            ;bootstrap       [method_handle invokestatic, [method_ref "java/lang/invoke/LambdaMetafactory", "metafactory", "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"]], [method_type "()V"], [method_handle invokestatic, [method_ref this, "makeFn$lambda", "()V"]], [method_type "()V"]


<init>:     method          "()V"
            stack           0x01
            locals          0x01
            aload_0
            invokespecial   [method_ref "java/lang/Object", "<init>", "()V"]
            return

main:       method          "([Ljava/lang/String;)V"
            flags           public, static, final
            stack           0x03
            locals          0x01
            invokestatic    [method_ref this, "makeFn", "()Ljava/lang/Runnable;"]
            dup
            getstatic       [field_ref "java/lang/System", "out", "Ljava/io/PrintStream;"]
            swap
            invokevirtual   [method_ref "java/io/PrintStream", "println", "(Ljava/lang/Object;)V"]
            invokeinterface [imethod_ref "java/lang/Runnable", "run", "()V"], 1
            return



makeFn:     method          "()Ljava/lang/Runnable;"
            flags           static
            stack           0x01
            locals          0x00
            invokedynamic   [dynamic_target [bootstrap [method_handle invokestatic, [method_ref "java/lang/invoke/LambdaMetafactory", "metafactory", "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"]], [method_type "()V"], [method_handle invokestatic, [method_ref this, "makeFn$lambda", "()V"]], [method_type "()V"]], "run", "()Ljava/lang/Runnable;"]
            areturn

makeFn$lambda:
            method          "()V"
            flags           static, synthetic
            stack           0x02
            locals          0x00
            getstatic       [field_ref "java/lang/System", "out", "Ljava/io/PrintStream;"]
            ldc             [string "Lambda!"]
            invokevirtual   [method_ref "java/io/PrintStream", "println", "(Ljava/lang/String;)V"]
            return
