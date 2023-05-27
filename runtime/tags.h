# pragma once

# define WORD_SIZE   8
# define TAG_SIZE    3
# define TAG_MASK    0b111

# define SPECIAL_TAG 0b000
# define INTEGER_TAG 0b001
# define ATOM_TAG    0b010
# define NIL_TAG     0b011
# define FUN_TAG     0b100
# define PID_TAG     0b101
# define MAP_TAG     0b110
# define BOX_TAG     0b111

# define UNBOUND_VALUE   0b0000
# define TIMED_OUT_VALUE 0b1000

# define TUPLE_HEADER   0x01
# define CONS_HEADER    0x02
# define CLOSURE_HEADER 0x03

# define FUN_KIND_STATIC  0x01
# define FUN_KIND_CLOSURE 0x02
