# ifdef __APPLE__

#   define ASM_FUN(name) \
      ".align 16         \n" \
      ".globl _" #name " \n" \
      "_" #name ":       \n"

# else

#   define ASM_FUN(name) \
      ".align 16        \n" \
      ".globl " #name " \n" \
      "" #name ":       \n"

# endif
