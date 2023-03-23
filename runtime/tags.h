# define WORD_SIZE   8
# define TAG_SIZE    3
# define TAG_MASK    0b111
# define NUMBER_TAG  0b000
# define UNBOUND_TAG 0b001
# define ATOM_TAG    0b010
# define NIL_TAG     0b011
# define FUN_TAG     0b100
# define PID_TAG     0b101
# define ____TAG     0b110
# define BOX_TAG     0b111

# define TUPLE_HEADER 0x01
# define CONS_HEADER 0x02