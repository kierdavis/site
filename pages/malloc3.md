---
title: The malloc3 algorithm for memory allocation
---

`malloc3` is a small system for managing memory allocations, much like the
UNIX/Linux `malloc` system call.

The system divides up the heap space into chunks, maintaining a linked list of
empty chunks. A cleanup process is run periodically, which joins together
adjacent empty chunks.

## Glossary

`N`
  : A notation used in this document to refer to the number of bytes in a
    machine word.

heap
  : The region of memory that is controlled by this algorithm.

address
  : An `N`-byte integer that uniquely references a byte within the *heap*.

null
  : An `N`-byte value that is different from any value that represents a valid
    address in the heap. The same value must be used for all occurrences of
    *null*, in the context of this allocation system.

chunk
  : A section of the heap that consists of a *chunk header* followed by a
    *chunk body*.

chunk header
  : A single `N`-byte word that represents the size of the *chunk body*, in
    bytes.

chunk body
  : A sequence of bytes of length determined by the corresponding *chunk header*.
    Addresses returned by `malloc` and passed to `free` are pointers to chunk
    bodies.

allocated chunk
  : A *chunk* that has been returned by `malloc` and is now in control of user
    code.

free chunk
  : Either a *chunk* that has never been allocated, or a chunk that has been
    allocated and then returned to the allocation system by passing the chunk
    pointer to `free`.

free chunk list
  : A linked list of free *chunks*. A pointer to the first chunk in the list 
    (the head of the list) is stored in the *head pointer* variable. Each chunk
    contains a pointer to the next free chunk in the list in the first word of
    the *chunk body* (the second word of the whole chunk). Storing meaningful
    data into a region normally reserved for usage by user code does not cause a
    problem, since a chunk is never under the control of user code and in the
    free chunk list at the same time.

head pointer
  : An `N`-byte variable/register/memory location holding a pointer to the first
    chunk in the free chunk list.

next-pointer
  : An `N`-byte pointer to the next chunk in the free chunk list. It is stored
    in first word of a chunk's body.

slice
  : The act of splitting a prospective chunk into two smaller chunks during
    allocation.

use
  : The act of allocating an unsplittable chunk.

## Overview of the system

### Initialisation

When the system is started, a short initialisation procedure must occur. This
consists of three steps:

  * Setting the head pointer to point to the start of the heap.
  * Setting the first `N`-byte word in the heap to the size of heap minus `N`.
  * Setting the second `N`-byte word in the heap to `null`.

These steps create a single chunk in the heap, starting at the first available
address and extending to the end of the heap. The head pointer is set to point
to this chunk. The length of this chunk is the size of the heap minus `N`, since
it is the length of just the chunk body (which contains the word set in the
third step, but not the word set in the second step). The next-pointer for this
chunk is set to `null` to mark the end of the list.

On a machine with N = 2 bytes and the heap extending from address `0000` to
address `FFFF` inclusive, the memory layout would now look like this:

    head ---> [0000] FFFE  (chunk 0000 header: length of body)
              [0002] null  (chunk 0000 body: next-pointer)
              [0004] ????  (chunk 0000 body)
              ....
              [FFFE] ????  (chunk 0000 body)

### Allocation

The first step in allocation is to round the requested allocation size up to the
next multiple of `N`. This ensures that all chunks start on `N`-byte boundaries,
which on some machines speeds up multibyte memory accesses.

The algorithm then iterates through the free chunk list. The size of each chunk
is fetched and compared against the requested allocation size.

  * If the chunk size is greater than the requested allocation size by at least
    a set number (`Z`) of words (i.e. if `chunksize >= reqsize + Z*N`), a
    *slice* occurs.
  * Otherwise, if the chunk size is greater than the requested allocation size
    (i.e. if `chunksize >= reqsize`), a *use* occurs.
  * Otherwise, the chunk is considered too small and the next chunk in the free
    chunk list is examined.

If the end of the free chunk list is reached without either a *slice* or *use*
event occurring, then there are no free chunks large enough and so the system
can be considered to be *out of memory*. The system may run a *coalesce* or a
*sorted coalesce* (described in later sections) and then try the allocation
algorithm again before reporting an error condition.

#### slice

A *slice* event means that the current chunk is fairly large in comparison to
the requested allocation size, and so the chunk will be split into two. One will
be used as the return value from `malloc` whilst the other will be returned to
the free chunk list.

The parameter `Z` (seen in the condition expression in the list above)
determines the minimum number of words that the size of the current chunk must
exceed the allocation size by. It can be assigned any value greater than one
(since a chunk must have a total size of at least two words). A typical value
for a processor with a small amount of memory is 9 (to leave room for a chunk
with an 8 word body).

A slice proceeds by calculating the value of a *slice pointer*, equal to the
current chunk pointer, plus `N` for the length of the chunk header, plus the
allocation size in bytes. The newly created chunk that the slice pointer refers
to will be called the *secondary sliced chunk*; the parent chunk will be
reduced in length and termed the *primary sliced chunk*. Four memory store
operations then take place (in any order):

  * The size of the secondary sliced chunk is set to the parent chunk size minus
    `N` for the length of the parent chunk header, minus the requested
    allocation size.
  * The next-pointer of the secondary sliced chunk is set to the next-pointer of
    the parent chunk.
  * The size of the primary sliced chunk is set to the requested allocation
    size.
  * The next-pointer that was followed to get to the parent chunk is updated to
    point to the secondary sliced chunk (skipping the primary sliced chunk,
    since that will be returned from `malloc`). If the parent chunk is the first
    one in the free chunk list, the head pointer is instead updated to point
    to the secondary sliced chunk.

Finally, a pointer to the body of the primary sliced chunk is returned as the
address of the allocated data.

##### Example

Suppose a portion of the free chunk list on a machine with `N` equal to 2 words
is as follows:

    ... ----> [5500] 0010  (length)
              [5502] --------------------------------> [3400] 0080  (length)
              [5504] ????  (leftover user data)        [3402] --------------------------------> ...
              ...                                      [3404] ????  (leftover user data)
              [5510] ????  (leftover user data)        ...
                                                       [3480] ????  (leftover user data)

A user wants to allocate data of size `0x20` bytes. The chunk at address
`0x3400` is satisfies the criteria for a split.

The slice pointer in this case will be `0x3422`, since it is the parent chunk
address (`0x3400`) plus the length of the primary sliced chunk's header
(2 bytes) plus the length of the primary sliced chunks' body (equal to the
requested allocation size, `0x20` bytes). 

    ... ----> [5500] 0010  (length)
              [5502] --------------------------------> [3400] 0080  (length)
              [5504] ????  (leftover user data)        [3402] --------------------------------> ...
              ...                                      [3404] ????  (leftover user data)
              [5510] ????  (leftover user data)        ...
                                                       [3420] ????  (leftover user data)
                                (slice pointer ---->)  [3422] ????  (leftover user data)
                                                       [3424] ????  (leftover user data)
                                                       ...
                                                       [3480] ????  (leftover user data)

The size of the secondary sliced chunk is set to `0x5e`, 

#### use
