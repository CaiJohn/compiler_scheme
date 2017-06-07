#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

// Often misunderstood static global variables in C are not
// accessible to code outside of the module.
// No one besides the collector ever needs to know tospace exists.
static int64_t* tospace_begin;
static int64_t* tospace_end;

// initialized it set during initialization of the heap, and can be
// checked in order to ensure that initialization has occurred.
static int initialized = 0;

/*
  Object Tag (64 bits)
  #b|- 7 bit unused -|- 50 bit field [50, 0] -| 6 bits length -| 1 bit isNotForwarding Pointer  
  * If the bottom-most bit is zero, the tag is really a forwarding pointer.
  * Otherwise, its an object tag. In that case, the next 
    6 bits give the length of the object (max of 50 64-bit words).
    The next 50 bits say where there are pointers.
    A '1' is a pointer, a '0' is not a pointer.
*/
static const int TAG_IS_NOT_FORWARD_MASK = 1;
static const int TAG_LENGTH_MASK = 126;
static const int TAG_LENGTH_RSHIFT = 1;
static const int TAG_PTR_BITFIELD_RSHIFT = 7;

// Check to see if a tag is actually a forwarding pointer.
static inline int is_forwarding(int64_t tag) {
  return !(tag & TAG_IS_NOT_FORWARD_MASK);
}

// Get the length field out of a tag.
static inline int get_length(int64_t tag){
  return (tag & TAG_LENGTH_MASK) >> TAG_LENGTH_RSHIFT;
}

// Get the "is pointer bitfield" out of a tag.
static inline int64_t get_ptr_bitfield(int64_t tag){
  return tag >> TAG_PTR_BITFIELD_RSHIFT;
}

// Read an integer from stdin
int64_t read_int() {
  int64_t i;
  scanf("%" SCNd64, &i);
  return i;
}

// print an integer to stdout
void print_int(int64_t x) {
  printf("%" PRId64, x);
}

// print a bool to stdout
void print_bool(int64_t x) {
  if (x){
    printf("#t");
  } else {
    printf("#f");
  }
}

void print_void() {
  printf("#<void>");
}

void print_vecbegin() {
  printf("#(");
}

void print_space() {
  printf(" ");
}

void print_vecend() {
  printf(")");
}

void print_ellipsis() {
  printf("#(...)");
}

#define ANY_TAG_MASK 3
#define ANY_TAG_LEN 2
#define ANY_TAG_INT 0
#define ANY_TAG_BOOL 1
#define ANY_TAG_VEC 2
#define ANY_TAG_FUN 3

int any_tag(int64_t any) {
  return any & ANY_TAG_MASK;
}

/* to do: need to cycle detection. -Jeremy */
void print_any(int64_t any) {
  switch (any_tag(any)) {
  case ANY_TAG_INT:
    printf("%" PRId64, any >> ANY_TAG_LEN);
    break;
  case ANY_TAG_BOOL:
    if (any >> ANY_TAG_LEN) {
      printf("#t");
    } else {
      printf("#f");
    }
    break;
  case ANY_TAG_VEC: {
    int64_t* vector_ptr = (int64_t*) (any & ~ANY_TAG_MASK);
    int64_t tag = vector_ptr[0];
    unsigned char len = get_length(tag);
    printf("#(");
    for (int i = 0; i != len; ++i) {
      print_any(vector_ptr[i + 1]);
    }
    printf(")");
    break;
  }
  case ANY_TAG_FUN:
    printf("#<procedure>");
    break;
  }
}
